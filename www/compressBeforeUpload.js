/**
 * Compress selected files in the browser before Shiny fileInput uploads.
 *
 * Helps a lot for STORE-only .results.zip archives and raw CSVs (same issue
 * as JSZip defaulting to STORE when building Pavlovia downloads).
 * Already-DEFLATE zips are left alone when savings would be tiny.
 */
(function () {
  var MIN_BYTES_TO_CONSIDER = 100 * 1024; // 100 KB
  var MIN_SAVINGS_RATIO = 0.05; // keep result only if >=5% smaller
  var ZIP_LOCAL_SIG = 0x04034b50;

  function formatBytes(n) {
    if (!isFinite(n) || n < 0) return "";
    if (n < 1024) return Math.round(n) + " B";
    if (n < 1024 * 1024) return (n / 1024).toFixed(1) + " KB";
    if (n < 1024 * 1024 * 1024) {
      return (n / (1024 * 1024)).toFixed(1) + " MB";
    }
    return (n / (1024 * 1024 * 1024)).toFixed(2) + " GB";
  }

  function updateProgress(pct, detail, title) {
    if (window.EasyEyesFileProgress && EasyEyesFileProgress.setUploading) {
      EasyEyesFileProgress.setUploading(pct, detail);
    }
    var bar = document.getElementById("file-progress-bar");
    var pctEl = document.getElementById("file-progress-pct");
    var detailEl = document.getElementById("file-progress-detail");
    var h2 =
      document.querySelector(".sweet-alert h2") ||
      document.querySelector(".swal-title");
    if (typeof pct === "number" && bar) {
      bar.style.width = Math.max(0, Math.min(100, pct)) + "%";
    }
    if (pctEl && typeof pct === "number") {
      pctEl.innerText = Math.round(pct) + "%";
    }
    if (detailEl && detail) detailEl.innerText = detail;
    if (h2 && title) h2.textContent = title;
  }

  function extOf(name) {
    var m = String(name || "")
      .toLowerCase()
      .match(/\.([a-z0-9]+)$/);
    return m ? m[1] : "";
  }

  function totalSize(files) {
    var t = 0;
    for (var i = 0; i < files.length; i++) t += files[i].size || 0;
    return t;
  }

  /**
   * Scan ZIP local-file headers. Returns true if most entries are STORE
   * (method 0), i.e. recompression is likely worthwhile.
   */
  function zipLooksMostlyStored(arrayBuffer) {
    var view = new DataView(arrayBuffer);
    var offset = 0;
    var store = 0;
    var other = 0;
    var scanned = 0;
    while (offset + 30 <= view.byteLength && scanned < 80) {
      var sig = view.getUint32(offset, true);
      if (sig !== ZIP_LOCAL_SIG) break;
      var method = view.getUint16(offset + 8, true);
      var flags = view.getUint16(offset + 6, true);
      var compSize = view.getUint32(offset + 18, true);
      var nameLen = view.getUint16(offset + 26, true);
      var extraLen = view.getUint16(offset + 28, true);
      scanned += 1;
      if (method === 0) store += 1;
      else other += 1;
      // Data descriptor (bit 3): sizes may be zero in local header; stop.
      if (flags & 0x8) break;
      offset += 30 + nameLen + extraLen + compSize;
    }
    if (scanned === 0) return true;
    return store >= other;
  }

  function readFileSlice(file, maxBytes) {
    var end = Math.min(file.size, maxBytes || 4 * 1024 * 1024);
    return file.slice(0, end).arrayBuffer();
  }

  function shouldTryCompress(files) {
    if (!files || !files.length) return false;
    if (typeof JSZip === "undefined") {
      console.warn("EasyEyes: JSZip not loaded; skipping pre-upload compress.");
      return false;
    }
    if (totalSize(files) < MIN_BYTES_TO_CONSIDER) return false;
    return true;
  }

  function addFileToZip(zip, file, path) {
    return file.arrayBuffer().then(function (buf) {
      zip.file(path || file.name, buf, {
        date: file.lastModified ? new Date(file.lastModified) : undefined
      });
    });
  }

  function generateDeflatedZip(zip, onMeta) {
    return zip.generateAsync(
      {
        type: "blob",
        compression: "DEFLATE",
        compressionOptions: { level: 6 }
      },
      function (meta) {
        if (onMeta) onMeta(meta);
      }
    );
  }

  /**
   * Re-pack an existing ZIP with DEFLATE, dropping __MACOSX / AppleDouble.
   */
  function recompressExistingZip(file, onProgress) {
    return JSZip.loadAsync(file).then(function (src) {
      var out = new JSZip();
      var names = Object.keys(src.files).filter(function (name) {
        var entry = src.files[name];
        if (entry.dir) return false;
        if (name.indexOf("__MACOSX") !== -1) return false;
        if (name.split("/").pop().indexOf("._") === 0) return false;
        return true;
      });
      var i = 0;
      var chain = Promise.resolve();
      names.forEach(function (name) {
        chain = chain.then(function () {
          return src.files[name].async("uint8array").then(function (data) {
            out.file(name, data, {
              date: src.files[name].date
            });
            i += 1;
            if (onProgress) {
              onProgress(
                (i / Math.max(names.length, 1)) * 50,
                "Recompressing " + i + "/" + names.length + ": " + name
              );
            }
          });
        });
      });
      return chain.then(function () {
        return generateDeflatedZip(out, function (meta) {
          if (onProgress) {
            onProgress(
              50 + (meta.percent || 0) / 2,
              "Writing compressed ZIP (" +
                Math.round(meta.percent || 0) +
                "%)..."
            );
          }
        });
      });
    });
  }

  /**
   * Pack loose CSV/XLSX files into one DEFLATE ZIP.
   */
  function packLooseFiles(files, onProgress) {
    var zip = new JSZip();
    var list = Array.prototype.slice.call(files);
    var i = 0;
    var chain = Promise.resolve();
    list.forEach(function (file) {
      chain = chain.then(function () {
        return addFileToZip(zip, file, file.name).then(function () {
          i += 1;
          if (onProgress) {
            onProgress(
              (i / Math.max(list.length, 1)) * 50,
              "Adding " + i + "/" + list.length + ": " + file.name
            );
          }
        });
      });
    });
    return chain.then(function () {
      return generateDeflatedZip(zip, function (meta) {
        if (onProgress) {
          onProgress(
            50 + (meta.percent || 0) / 2,
            "Compressing ZIP (" + Math.round(meta.percent || 0) + "%)..."
          );
        }
      });
    });
  }

  function assignFilesAndRetrigger(input, fileList) {
    var dt = new DataTransfer();
    for (var i = 0; i < fileList.length; i++) {
      dt.items.add(fileList[i]);
    }
    input.files = dt.files;
    input.dataset.easyeyesPrecompressed = "1";
    input.dispatchEvent(new Event("change", { bubbles: true }));
    // Clear flag shortly after so a later user pick runs compress again.
    window.setTimeout(function () {
      delete input.dataset.easyeyesPrecompressed;
    }, 0);
  }

  /**
   * @return {Promise<boolean>} true if this handler took over (caller should
   *   stop Shiny's original change handling until we re-dispatch).
   */
  function maybeCompressBeforeUpload(input, fileList) {
    var files = Array.prototype.slice.call(fileList || []);
    if (!shouldTryCompress(files)) {
      return Promise.resolve(false);
    }

    var originalBytes = totalSize(files);
    var allZip =
      files.length > 0 &&
      files.every(function (f) {
        return extOf(f.name) === "zip";
      });
    var allLoose =
      files.length > 0 &&
      files.every(function (f) {
        var e = extOf(f.name);
        return e === "csv" || e === "xlsx" || e === "xls";
      });

    if (!allZip && !allLoose) {
      return Promise.resolve(false);
    }

    updateProgress(
      0,
      "Checking whether browser compression will help (" +
        formatBytes(originalBytes) +
        ")...",
      "Compressing before upload..."
    );

    var work = Promise.resolve(null);

    if (allLoose) {
      work = packLooseFiles(files, function (pct, detail) {
        updateProgress(pct, detail, "Compressing before upload...");
      }).then(function (blob) {
        var base =
          files.length === 1
            ? String(files[0].name).replace(/\.[^.]+$/, "")
            : "easyeyes_upload";
        return new File([blob], base + ".results.zip", {
          type: "application/zip",
          lastModified: Date.now()
        });
      });
    } else if (allZip && files.length === 1) {
      work = readFileSlice(files[0], 8 * 1024 * 1024).then(function (buf) {
        if (!zipLooksMostlyStored(buf)) {
          updateProgress(
            0,
            "ZIP already compressed; uploading as-is (" +
              formatBytes(originalBytes) +
              ").",
            "Uploading file(s)..."
          );
          return null;
        }
        return recompressExistingZip(files[0], function (pct, detail) {
          updateProgress(pct, detail, "Compressing before upload...");
        }).then(function (blob) {
          return new File([blob], files[0].name, {
            type: "application/zip",
            lastModified: Date.now()
          });
        });
      });
    } else if (allZip) {
      // Multiple ZIPs: recompress each STORE zip independently.
      var outFiles = [];
      var idx = 0;
      work = files.reduce(function (chain, file) {
        return chain.then(function () {
          idx += 1;
          return readFileSlice(file, 8 * 1024 * 1024).then(function (buf) {
            if (!zipLooksMostlyStored(buf)) {
              outFiles.push(file);
              return;
            }
            return recompressExistingZip(file, function (pct, detail) {
              updateProgress(
                ((idx - 1) / files.length) * 100 + pct / files.length,
                "File " + idx + "/" + files.length + " — " + detail,
                "Compressing before upload..."
              );
            }).then(function (blob) {
              var compressed = new File([blob], file.name, {
                type: "application/zip",
                lastModified: Date.now()
              });
              if (
                compressed.size <
                file.size * (1 - MIN_SAVINGS_RATIO)
              ) {
                outFiles.push(compressed);
              } else {
                outFiles.push(file);
              }
            });
          });
        });
      }, Promise.resolve()).then(function () {
        return outFiles;
      });
    }

    return work
      .then(function (result) {
        if (!result) return false;

        var newFiles = Array.isArray(result) ? result : [result];
        var newBytes = totalSize(newFiles);
        if (newBytes >= originalBytes * (1 - MIN_SAVINGS_RATIO)) {
          updateProgress(
            0,
            "Compression saved little; uploading original (" +
              formatBytes(originalBytes) +
              ").",
            "Uploading file(s)..."
          );
          return false;
        }

        updateProgress(
          100,
          "Compressed " +
            formatBytes(originalBytes) +
            " → " +
            formatBytes(newBytes) +
            ". Starting upload...",
          "Uploading file(s)..."
        );
        console.info(
          "[EasyEyes] Pre-upload compress:",
          formatBytes(originalBytes),
          "→",
          formatBytes(newBytes),
          "(" +
            Math.round((1 - newBytes / originalBytes) * 100) +
            "% smaller)"
        );
        assignFilesAndRetrigger(input, newFiles);
        return true;
      })
      .catch(function (err) {
        console.error("[EasyEyes] Pre-upload compress failed:", err);
        updateProgress(
          0,
          "Compression failed; uploading original files...",
          "Uploading file(s)..."
        );
        return false;
      });
  }

  window.EasyEyesCompressUpload = {
    maybeCompressBeforeUpload: maybeCompressBeforeUpload,
    formatBytes: formatBytes
  };
})();
