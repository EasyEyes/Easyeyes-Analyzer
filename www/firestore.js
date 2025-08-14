document.addEventListener("DOMContentLoaded", async function () {
  // window.alert = function (message) {
  //   // Do nothing or log the message to console
  //   console.log("Alert suppressed:", message);
  // };
  // console.error = function () {
  //   // Do nothing or log the message to console
  //   console.log("Error suppressed:", arguments);
  // };

  // // Override console.warn to suppress warning messages
  // console.warn = function () {
  //   // Do nothing or log the message to console
  //   console.log("Warning suppressed:", arguments);
  // };
  let toShiny;
  let toShinyOptions;
  let checkBoxs = [];
  document.getElementById("profilePlot").innerHTML =
    "<p style = 'font-size:20px'> Retrieving database menu <p> <br> <div class='loader-profile'></div>";
  const app = firebase.initializeApp(firebaseConfig);
  const db = firebase.firestore();

  function addToMap(map, value) {
    if (value) {
      const lowercaseValue = value.toLowerCase();
      if (!map.has(lowercaseValue)) {
        map.set(lowercaseValue, new Set());
      }
      map.get(lowercaseValue).add(value);
    } else {
      return;
    }
  }
  function aspectRatio(a, b) {
    // Euclidean algorithm
    if (a && b) {
      return `${Math.max(a, b) / Math.min(a, b)}:1`;
    } else {
      return "NA";
    }
  }

  function getScreenPx(a, b) {
    if (a && b) {
      return `${a}x${b}`;
    } else {
      return "NA";
    }
  }

  let gcdN;
  var micOEMs = new Set();
  var micModelNames = new Set();
  var micIDs = new Set();
  var mic51IDs = new Set();
  var micScreenPx = new Set();
  var micAspectRatio = new Set();

  var micOEMsMap = new Map();
  var micModelNamesMap = new Map();
  var micIDsMap = new Map();

  var mics = db.collection("Microphones");
  var loudspeakers = db.collection("Loudspeakers");
  
  console.log('Start to get mics');
  await mics.get().then((querySnapshot) => {
    querySnapshot.forEach((doc) => {
      addToMap(micOEMsMap, doc.data().OEM);
      addToMap(micModelNamesMap, doc.data().micModelName);
      addToMap(micIDsMap, doc.data().ID);
      mic51IDs.add(doc.data().ID_from_51Degrees);
      micScreenPx.add(
        getScreenPx(doc.data().screenWidth, doc.data().screenHeight),
      );
      micAspectRatio.add(
        aspectRatio(doc.data().screenWidth, doc.data().screenHeight),
      );
    });
  });
  
  micOEMsMap.forEach((values, lowercaseOEM) => {
    const [firstCaseOEM] = values;
    if (firstCaseOEM) {
      micOEMs.add(firstCaseOEM);
    }
  });
  micModelNamesMap.forEach((values, micModelName) => {
    const [firstCaseMicModelName] = values;
    if (firstCaseMicModelName) {
      micModelNames.add(firstCaseMicModelName);
    }
  });
  micIDsMap.forEach((values, lowercaseID) => {
    const [firstCaseID] = values;
    if (firstCaseID) {
      micIDs.add(firstCaseID);
    }
  });
  console.log('Done mics');

  var speakerOEMs = new Set();
  var speakerModelNames = new Set();
  var speakerIDs = new Set();
  var speaker51IDs = new Set();
  var speakerScreenPx = new Set();
  var speakerAspectRatio = new Set();

  var speakerOEMsMap = new Map();
  var speakerModelNamesMap = new Map();
  var speakerIDsMap = new Map();

  console.log('Start Speakers');
 await loudspeakers
  .orderBy('CalibrationDate', 'desc') 
  .limit(200) // Adjust this as needed
  .get()
  .then((querySnapshot) => {
    querySnapshot.forEach((doc) => {
      addToMap(speakerOEMsMap, doc.data().OEM);
      addToMap(speakerModelNamesMap, doc.data().fullLoudspeakerModelName);
      addToMap(speakerIDsMap, doc.data().fullLoudspeakerModelNumber);
      speaker51IDs.add(doc.data().DeviceId);
      speakerScreenPx.add(
        getScreenPx(doc.data().screenWidth, doc.data().screenHeight),
      );
      speakerAspectRatio.add(
        aspectRatio(doc.data().screenWidth, doc.data().screenHeight),
      );
    });
  });

  speakerOEMsMap.forEach((values, lowercaseOEM) => {
    const [firstCaseOEM] = values;
    if (firstCaseOEM) {
      speakerOEMs.add(firstCaseOEM);
    }
  });
  speakerModelNamesMap.forEach((values, speakerModelName) => {
    const [firstCaseMicModelName] = values;
    if (firstCaseMicModelName) {
      speakerModelNames.add(firstCaseMicModelName);
    }
  });
  speakerIDsMap.forEach((values, lowercaseID) => {
    const [firstCaseID] = values;
    if (firstCaseID) {
      speakerIDs.add(firstCaseID);
    }
  });
  
  console.log('Done speakers');
  const SDToleranceContainer = document.getElementById("SDTolerance");
  const filterBoolContainer = document.getElementById("filterBool");
  const deviceBoolContainer = document.getElementById("deviceBool");
  const screenBoolContainer = document.getElementById("screenBool");
  const DeviceIDContainer = document.getElementById("deviceID");
  const screenPxContainer = document.getElementById("screenPx");
  const aspectRatioContainer = document.getElementById("aspectRatio");

  document.getElementById("deviceBool").addEventListener("change", function () {
    if (this.checked) {
      document.getElementById("screenBool").checked = false;
    }
  });

  document.getElementById("screenBool").addEventListener("change", function () {
    if (this.checked) {
      document.getElementById("deviceBool").checked = false;
    }
  });

  filterBoolContainer.checked = false;
  SDToleranceContainer.value = 10.0;
  const transducerSelect = document.getElementById("transducer");
  transducerSelect.innerHTML = "";
  let microphoneOption = document.createElement("option");
  microphoneOption.value = "Microphones";
  microphoneOption.text = "Microphones";
  transducerSelect.add(microphoneOption);
  let loudspeakerOption = document.createElement("option");
  loudspeakerOption.value = "Loudspeakers";
  loudspeakerOption.text = "Loudspeakers";
  transducerSelect.add(loudspeakerOption);

  const OEMSelect = document.getElementById("OEM");
  const modelsContainer = document.getElementById("Model");
  const IDcontainers = document.getElementById("IDs");

  function updateOEMsContainer() {
    OEMSelect.innerHTML = "";
    let OEMelement = document.createElement("option");
    OEMelement.value = "All";
    OEMelement.text = "All";
    OEMSelect.add(OEMelement);

    if (transducerSelect.value == "Microphones") {
      micOEMs.forEach((option) => {
        const optionElement = document.createElement("option");
        optionElement.value = option;
        optionElement.text = option;
        OEMSelect.add(optionElement);
      });
    } else {
      speakerOEMs.forEach((option) => {
        const optionElement = document.createElement("option");
        optionElement.value = option;
        optionElement.text = option;
        OEMSelect.add(optionElement);
      });
    }
  }

  function updateModelsContainer() {
    modelsContainer.innerHTML = "";
    let modelElement = document.createElement("option");
    modelElement.value = "All";
    modelElement.text = "All";
    modelsContainer.add(modelElement);
    if (transducerSelect.value == "Microphones") {
      micModelNames.forEach((option) => {
        const optionElement = document.createElement("option");
        optionElement.value = option;
        optionElement.text = option;
        modelsContainer.add(optionElement);
      });
    } else {
      speakerModelNames.forEach((option) => {
        const optionElement = document.createElement("option");
        optionElement.value = option;
        optionElement.text = option;
        modelsContainer.add(optionElement);
      });
    }
  }

  function updateIDContainer() {
    IDcontainers.innerHTML = "";
    let IDElement = document.createElement("option");
    IDElement.value = "All";
    IDElement.text = "All";
    IDcontainers.add(IDElement);
    if (transducerSelect.value == "Microphones") {
      micIDs.forEach((option) => {
        const optionElement = document.createElement("option");
        optionElement.value = option;
        optionElement.text = option;
        IDcontainers.add(optionElement);
      });
    } else {
      speakerIDs.forEach((option) => {
        const optionElement = document.createElement("option");
        optionElement.value = option;
        optionElement.text = option;
        IDcontainers.add(optionElement);
      });
    }
  }

  function updateDeviceIDContainer() {
    DeviceIDContainer.innerHTML = "";
    let deviceIDelement = document.createElement("option");
    deviceIDelement.value = "All";
    deviceIDelement.text = "All";
    DeviceIDContainer.add(deviceIDelement);

    if (transducerSelect.value == "Microphones") {
      mic51IDs.forEach((option) => {
        const optionElement = document.createElement("option");
        optionElement.value = option;
        optionElement.text = option;
        DeviceIDContainer.add(optionElement);
      });
    } else {
      speaker51IDs.forEach((option) => {
        const optionElement = document.createElement("option");
        optionElement.value = option;
        optionElement.text = option;
        DeviceIDContainer.add(optionElement);
      });
    }
  }

  function updateScreenPxContainer() {
    screenPxContainer.innerHTML = "";
    let screenPxElement = document.createElement("option");
    screenPxElement.value = "All";
    screenPxElement.text = "All";
    screenPxContainer.add(screenPxElement);

    if (transducerSelect.value == "Microphones") {
      micScreenPx.forEach((option) => {
        const optionElement = document.createElement("option");
        optionElement.value = option;
        optionElement.text = option;
        screenPxContainer.add(optionElement);
      });
    } else {
      speakerScreenPx.forEach((option) => {
        const optionElement = document.createElement("option");
        optionElement.value = option;
        optionElement.text = option;
        screenPxContainer.add(optionElement);
      });
    }
  }

  function updateAspectRatioContainer() {
    aspectRatioContainer.innerHTML = "";
    let aspectRatioElement = document.createElement("option");
    aspectRatioElement.value = "All";
    aspectRatioElement.text = "All";
    aspectRatioContainer.add(aspectRatioElement);

    if (transducerSelect.value == "Microphones") {
      micAspectRatio.forEach((option) => {
        const optionElement = document.createElement("option");
        optionElement.value = option;
        optionElement.text = option;
        aspectRatioContainer.add(optionElement);
      });
    } else {
      speakerAspectRatio.forEach((option) => {
        const optionElement = document.createElement("option");
        optionElement.value = option;
        optionElement.text = option;
        aspectRatioContainer.add(optionElement);
      });
    }
  }

  updateOEMsContainer();
  updateModelsContainer();
  updateIDContainer();
  updateDeviceIDContainer();
  updateScreenPxContainer();
  updateAspectRatioContainer();

  function sanitizeForId(input) {
    // Replace non-alphanumeric characters with underscores
    return String(input).replace(/[^a-zA-Z0-9-]/g, "-");
  }

  async function updatePlot(whatToPlot) {
    console.log(whatToPlot);
    // const selectedModels = Array.from(modelsContainer.querySelectorAll('input[type="checkbox"]:checked')).map((checkbox) => checkbox.value);
    document.getElementById("profilePlot").innerHTML =
      "<div class='loader-profile'></div>";
    const data = [];
    let i = 0;
    let query, selectedValue;
    let OEMquery, modelQuery, IDQuery;
    if (transducerSelect.value == "Microphones") {
      OEMquery =
        OEMSelect.value == "All"
          ? mics
          : mics.where(
              "OEM",
              "in",
              [...micOEMsMap.get(OEMSelect.value.toLowerCase())] || [],
            );
      modelQuery =
        modelsContainer?.value == "All"
          ? OEMquery
          : mics.where(
              "micModelName",
              "in",
              [...micModelNamesMap.get(modelsContainer.value.toLowerCase())] ||
                [],
            );
      IDQuery =
        IDcontainers.value == "All"
          ? modelQuery
          : mics.where(
              "ID",
              "in",
              [...micIDsMap.get(IDcontainers.value.toLowerCase())] || [],
            );
    } else {
      OEMquery =
        OEMSelect.value == "All"
          ? loudspeakers
          : loudspeakers.where(
              "OEM",
              "in",
              [...speakerOEMsMap.get(OEMSelect.value.toLowerCase())] || [],
            );
      modelQuery =
        modelsContainer.value == "All"
          ? OEMquery
          : loudspeakers.where(
              "fullLoudspeakerModelName",
              "in",
              [
                ...speakerModelNamesMap.get(
                  modelsContainer.value.toLowerCase(),
                ),
              ] || [],
            );
      IDQuery =
        IDcontainers.value == "All"
          ? modelQuery
          : loudspeakers.where(
              "fullLoudspeakerModelNumber",
              "in",
              [...speakerIDsMap.get(IDcontainers.value.toLowerCase())] || [],
            );
    }

    if (whatToPlot === "micOEM") {
      selectedValue = OEMSelect.value;
      query = OEMquery;
    } else if (whatToPlot === "micModelName") {
      selectedValue =
        modelsContainer.value == "All"
          ? OEMSelect.value
          : modelsContainer.value;
      query = modelQuery;
    } else if (whatToPlot === "micID" || whatToPlot === "speakerID") {
      selectedValue =
        IDcontainers.value == "All"
          ? modelsContainer.value
          : IDcontainers.value;
      query = IDQuery;
    } else if (whatToPlot == "micModelName") {
      selectedValue = DeviceIDContainer.value;
      query = loudspeakers.where("DeviceId", "==", DeviceIDContainer.value);
    } else if (whatToPlot === "speakerOEM") {
      selectedValue = OEMSelect.value;
      query = OEMquery;
    } else if (whatToPlot === "speakerModelName") {
      selectedValue =
        modelsContainer.value == "All"
          ? OEMSelect.value
          : modelsContainer.value;
      query = modelQuery;
    } else if (whatToPlot === "mic51ID") {
      selectedValue = DeviceIDContainer.value;
      query = mics.where("ID_from_51Degrees", "==", DeviceIDContainer.value);
    } else if (whatToPlot === "aspectRatio") {
      selectedValue = aspectRatioContainer.value;
      query = transducerSelect.value == "Microphones" ? mics : loudspeakers;
    } else {
      selectedValue = screenPxContainer.value;
      query = transducerSelect.value == "Microphones" ? mics : loudspeakers;
    }

    try {
      toShiny = {
        createDates: [],
        jsonFileName: [],
        OEMs: [],
        modelNames: [],
        modelNumbers: [],
        SDs: [],
        FilteredMLSRange: [],
        labels: [],
        title: [],
        isDefault: [],
        T: [],
        W: [],
        Q: [],
        gainDBSPL: [],
        speakerGain_dB: [],
        micGain_dB: [],
        backgroundDBSPL: [],
        RMSError: [],
        fs2: [],
      };
      checkBoxs = {
        id: [],
        fileName: [],
      };
      let j = 1;

      await query.get().then((querySnapshot) => {
        querySnapshot.forEach((doc) => {
          let label;

          if (whatToPlot === "micOEM") {
            label = `${doc.data().micModelName}/${doc.data().isDefault ? "default" : doc.data().DateText}`;
          } else if (whatToPlot === "micModelName") {
            label = `${doc.data().ID}/${doc.data().isDefault ? "default" : doc.data().DateText}`;
          } else if (whatToPlot === "micID") {
            label = doc.data().isDefault ? "default" : doc.data().DateText;
            // speakers
          } else if (whatToPlot === "speakerOEM") {
            label = `${doc.data().fullLoudspeakerModelName}/${doc.data().CalibrationDate}`;
          } else if (whatToPlot === "speakerModelName") {
            label = `${doc.data().fullLoudspeakerModelNumber}/${doc.data().CalibrationDate}`;
          } else if (whatToPlot === "speakerModelName") {
            label = doc.data().CalibrationDate;
          } else {
            label =
              transducerSelect.value == "Microphones"
                ? doc.data().DateText
                : doc.data().CalibrationDate;
          }

          const plotDevice =
            deviceBoolContainer.checked &&
            (!filterBoolContainer.checked ||
              doc.data().isDefault ||
              doc.data().componentCorrectionSD <= SDToleranceContainer.value) &&
            (doc.data().ir || doc.data().linear);

          let plotScreen =
            screenBoolContainer.checked &&
            (!filterBoolContainer.checked ||
              doc.data().isDefault ||
              doc.data().componentCorrectionSD <= SDToleranceContainer.value);

          if (screenPxContainer.value != "All") {
            plotScreen =
              plotScreen &&
              getScreenPx(doc.data().screenWidth, doc.data().screenHeight) ==
                screenPxContainer.value;
          } else if (aspectRatioContainer.value != "All") {
            plotScreen =
              plotScreen &&
              aspectRatio(doc.data().screenWidth, doc.data().screenHeight) ==
                aspectRatioContainer.value;
          }
          if (plotDevice || plotScreen) {
            // sent to shiny server to create datatable
            toShiny.createDates.push(
              transducerSelect.value == "Microphones"
                ? doc.data().DateText
                : doc.data().CalibrationDate,
            );
            toShiny.jsonFileName.push(doc.data()?.jsonFileName);
            toShiny.OEMs.push(doc.data()?.OEM || "unknown");
            toShiny.modelNames.push(
              transducerSelect.value == "Microphones"
                ? doc.data()?.micModelName
                : doc.data()?.fullLoudspeakerModelName,
            );
            toShiny.modelNumbers.push(
              transducerSelect.value == "Microphones"
                ? doc.data()?.ID || "unknown"
                : doc.data()?.fullLoudspeakerModelNumber || "unknown",
            );
            toShiny.SDs.push(doc.data()?.componentCorrectionSD || NaN);
            toShiny.FilteredMLSRange.push(
              doc.data()?.filteredMLSComponentMax || NaN,
            );
            toShiny.labels.push(label);
            toShiny.isDefault.push(doc.data()?.isDefault || false);
            toShiny.T.push(doc.data()?.T || NaN);
            toShiny.W.push(doc.data()?.W || NaN);
            toShiny.Q.push(doc.data()?.Q || NaN);
            toShiny.speakerGain_dB.push(
              transducerSelect.value == "Microphones"
                ? NaN
                : doc.data()?.gainDBSPL || NaN,
            );
            toShiny.micGain_dB.push(
              transducerSelect.value == "Microphones"
                ? doc.data()?.gainDBSPL
                : doc.data()?.micInfo.gainDBSPL || NaN,
            );
            toShiny.gainDBSPL.push(
              transducerSelect.value == "Microphones"
                ? NaN
                : doc.data()?.gainDBSPL + doc.data()?.micInfo.gainDBSPL,
            );
            toShiny.backgroundDBSPL.push(doc.data()?.backgroundDBSPL || NaN);
            toShiny.RMSError.push(doc.data()?.RMSError || NaN);
            toShiny.fs2.push(doc.data()?.fs2 || NaN);
            data.push(doc.data());
          }
        });
        Shiny.setInputValue("transducerType", transducerSelect.value);
        Shiny.setInputValue("plotTitle", "Profiles for " + selectedValue);
        Shiny.setInputValue("toShiny", JSON.stringify(toShiny));
        Shiny.setInputValue("totalData", JSON.stringify(data));
      });
    } catch (error) {
      console.error("Error updating plot:", error);
    }
  }

  async function updateOptions(whatToPlot) {
    document.getElementById("profilePlot").innerHTML =
      "<div class='loader-profile'></div>";
    const data = [];
    let i = 0;
    let query, selectedValue;
    let OEMquery, modelQuery, IDQuery;
    if (transducerSelect.value == "Microphones") {
      OEMquery =
        OEMSelect.value == "All"
          ? mics
          : mics.where(
              "OEM",
              "in",
              [...micOEMsMap.get(OEMSelect.value.toLowerCase())] || [],
            );
      modelQuery =
        modelsContainer?.value == "All"
          ? OEMquery
          : mics.where(
              "micModelName",
              "in",
              [...micModelNamesMap.get(modelsContainer.value.toLowerCase())] ||
                [],
            );
      IDQuery =
        IDcontainers.value == "All"
          ? modelQuery
          : mics.where(
              "ID",
              "in",
              [...micIDsMap.get(IDcontainers.value.toLowerCase())] || [],
            );
    } else {
      OEMquery =
        OEMSelect.value == "All"
          ? loudspeakers
          : loudspeakers.where(
              "OEM",
              "in",
              [...speakerOEMsMap.get(OEMSelect.value.toLowerCase())] || [],
            );
      modelQuery =
        modelsContainer.value == "All"
          ? OEMquery
          : loudspeakers.where(
              "fullLoudspeakerModelName",
              "in",
              [
                ...speakerModelNamesMap.get(
                  modelsContainer.value.toLowerCase(),
                ),
              ] || [],
            );
      IDQuery =
        IDcontainers.value == "All"
          ? modelQuery
          : loudspeakers.where(
              "fullLoudspeakerModelNumber",
              "in",
              [...speakerIDsMap.get(IDcontainers.value.toLowerCase())] || [],
            );
    }

    if (whatToPlot === "micOEM") {
      selectedValue = OEMSelect.value;
      query = OEMquery;
    } else if (whatToPlot === "micModelName") {
      selectedValue =
        modelsContainer.value == "All"
          ? OEMSelect.value
          : modelsContainer.value;
      query = modelQuery;
    } else if (whatToPlot === "micID" || whatToPlot === "speakerID") {
      selectedValue =
        IDcontainers.value == "All"
          ? modelsContainer.value
          : IDcontainers.value;
      query = IDQuery;
    } else if (whatToPlot == "micModelName") {
      selectedValue = DeviceIDContainer.value;
      query = loudspeakers.where("DeviceId", "==", DeviceIDContainer.value);
    } else if (whatToPlot === "speakerOEM") {
      selectedValue = OEMSelect.value;
      query = OEMquery;
    } else if (whatToPlot === "speakerModelName") {
      selectedValue =
        modelsContainer.value == "All"
          ? OEMSelect.value
          : modelsContainer.value;
      query = modelQuery;
    } else if (whatToPlot === "mic51ID") {
      selectedValue = DeviceIDContainer.value;
      query = mics.where("ID_from_51Degrees", "==", DeviceIDContainer.value);
    } else if (whatToPlot === "aspectRatio") {
      selectedValue = aspectRatioContainer.value;
      query = transducerSelect.value == "Microphones" ? mics : loudspeakers;
    } else {
      selectedValue = screenPxContainer.value;
      query = transducerSelect.value == "Microphones" ? mics : loudspeakers;
    }

    try {
      toShinyOptions = {
        createDates: [],
        jsonFileName: [],
        OEMs: [],
        modelNames: [],
        modelNumbers: [],
        SDs: [],
        FilteredMLSRange: [],
        labels: [],
        title: [],
        isDefault: [],
        T: [],
        W: [],
        Q: [],
        gainDBSPL: [],
        speakerGain_dB: [],
        micGain_dB: [],
        backgroundDBSPL: [],
        RMSError: [],
        fs2: [],
      };
      checkBoxs = {
        id: [],
        fileName: [],
      };
      let j = 1;

      await query.get().then((querySnapshot) => {
        querySnapshot.forEach((doc) => {
          let label;

          if (whatToPlot === "micOEM") {
            label = `${doc.data().micModelName}/${doc.data().isDefault ? "default" : doc.data().DateText}`;
          } else if (whatToPlot === "micModelName") {
            label = `${doc.data().ID}/${doc.data().isDefault ? "default" : doc.data().DateText}`;
          } else if (whatToPlot === "micID") {
            label = doc.data().isDefault ? "default" : doc.data().DateText;
            // speakers
          } else if (whatToPlot === "speakerOEM") {
            label = `${doc.data().fullLoudspeakerModelName}/${doc.data().CalibrationDate}`;
          } else if (whatToPlot === "speakerModelName") {
            label = `${doc.data().fullLoudspeakerModelNumber}/${doc.data().CalibrationDate}`;
          } else if (whatToPlot === "speakerModelName") {
            label = doc.data().CalibrationDate;
          } else {
            label =
              transducerSelect.value == "Microphones"
                ? doc.data().DateText
                : doc.data().CalibrationDate;
          }

          const plotDevice =
            deviceBoolContainer.checked &&
            (!filterBoolContainer.checked ||
              doc.data().isDefault ||
              doc.data().componentCorrectionSD <= SDToleranceContainer.value) &&
            (doc.data().ir || doc.data().linear);

          let plotScreen =
            screenBoolContainer.checked &&
            (!filterBoolContainer.checked ||
              doc.data().isDefault ||
              doc.data().componentCorrectionSD <= SDToleranceContainer.value);

          if (screenPxContainer.value != "All") {
            plotScreen =
              plotScreen &&
              getScreenPx(doc.data().screenWidth, doc.data().screenHeight) ==
                screenPxContainer.value;
          } else if (aspectRatioContainer.value != "All") {
            plotScreen =
              plotScreen &&
              aspectRatio(doc.data().screenWidth, doc.data().screenHeight) ==
                aspectRatioContainer.value;
          }
          if (plotDevice || plotScreen) {
            // sent to shiny server to create datatable
            toShinyOptions.createDates.push(
              transducerSelect.value == "Microphones"
                ? doc.data().DateText
                : doc.data().CalibrationDate,
            );
            toShinyOptions.jsonFileName.push(doc.data()?.jsonFileName);
            toShinyOptions.OEMs.push(doc.data()?.OEM || "unknown");
            toShinyOptions.modelNames.push(
              transducerSelect.value == "Microphones"
                ? doc.data()?.micModelName
                : doc.data()?.fullLoudspeakerModelName,
            );
            toShinyOptions.modelNumbers.push(
              transducerSelect.value == "Microphones"
                ? doc.data()?.ID || "unknown"
                : doc.data()?.fullLoudspeakerModelNumber || "unknown",
            );
            toShinyOptions.SDs.push(doc.data()?.componentCorrectionSD || NaN);
            toShinyOptions.FilteredMLSRange.push(
              doc.data()?.filteredMLSComponentMax || NaN,
            );
            toShinyOptions.labels.push(label);
            toShinyOptions.isDefault.push(doc.data()?.isDefault || false);
            toShinyOptions.T.push(doc.data()?.T || NaN);
            toShinyOptions.W.push(doc.data()?.W || NaN);
            toShinyOptions.Q.push(doc.data()?.Q || NaN);
            toShinyOptions.speakerGain_dB.push(
              transducerSelect.value == "Microphones"
                ? NaN
                : doc.data()?.gainDBSPL || NaN,
            );
            toShinyOptions.micGain_dB.push(
              transducerSelect.value == "Microphones"
                ? doc.data()?.gainDBSPL
                : doc.data()?.micInfo.gainDBSPL || NaN,
            );
            toShinyOptions.gainDBSPL.push(
              transducerSelect.value == "Microphones"
                ? NaN
                : doc.data()?.gainDBSPL + doc.data()?.micInfo.gainDBSPL,
            );
            toShinyOptions.backgroundDBSPL.push(
              doc.data()?.backgroundDBSPL || NaN,
            );
            toShinyOptions.RMSError.push(doc.data()?.RMSError || NaN);
            toShinyOptions.fs2.push(doc.data()?.fs2 || NaN);
            data.push(doc.data());
          }
        });
        Shiny.setInputValue("transducerType", transducerSelect.value);
        Shiny.setInputValue("toShinyOptions", JSON.stringify(toShinyOptions));
        console.log("done");
        Shiny.addCustomMessageHandler("options", function (message) {
          console.log(message);
          document.getElementById("profilePlot").display = false;
          document.getElementById("profilePlot").innerHTML = "";
        });
      });
    } catch (error) {
      console.error("Error updating plot:", error);
    }
  }

  function refreshShinyPlot() {
    const dataTableElement = $("#dataTable");

    // Get the data of the selected rows
    const selectedRowsData = $(dataTableElement)
      .DataTable()
      .rows(".selected")
      .data()
      .toArray();
    console.log($(dataTableElement));
    // Extract jsonFileName values from the selected rows
    const jsonFileNames = selectedRowsData.map((row) => row[1]); // Assuming jsonFileName is at index 1
  }

  // event listeners
  transducerSelect.addEventListener("change", () => {
    updateOEMsContainer();
    updateModelsContainer();
    updateIDContainer();
    updateDeviceIDContainer();
    updateScreenPxContainer();
    updateAspectRatioContainer();
  });

  document.getElementById("plotButton").onclick = async () => {
    document.getElementById("profilePlot").innerHTML =
      "<div class='loader-profile'></div>";
    document.getElementById("shiftedProfilePlot").innerHTML = "";
    if (
      transducerSelect.value == "Microphones" &&
      deviceBoolContainer.checked
    ) {
      if (DeviceIDContainer.value != "All") {
        await updatePlot("mic51ID");
      } else if (IDcontainers.value != "All") {
        await updatePlot("micID");
      } else if (modelsContainer.value != "All") {
        await updatePlot("micModelName");
      } else {
        await updatePlot("micOEM");
      }
    } else if (
      transducerSelect.value == "Loudspeakers" &&
      deviceBoolContainer.checked
    ) {
      if (DeviceIDContainer.value != "All") {
        await updatePlot("speaker51ID");
      } else if (IDcontainers.value != "All") {
        await updatePlot("speakerID");
      } else if (modelsContainer.value != "All") {
        await updatePlot("speakerModelName");
      } else {
        await updatePlot("speakerOEM");
      }
    } else if (screenBoolContainer.checked) {
      if (aspectRatioContainer.value != "All") {
        await updatePlot("aspectRatio");
      } else {
        await updatePlot("screenPx");
      }
    }
  };

  document.getElementById("refreshButton").onclick = async () => {
    document.getElementById("shiftedProfilePlot").display = false;
    document.getElementById("shiftedProfilePlot").innerHTML = "";
    if (
      transducerSelect.value == "Microphones" &&
      deviceBoolContainer.checked
    ) {
      if (DeviceIDContainer.value != "All") {
        await updateOptions("mic51ID");
      } else if (IDcontainers.value != "All") {
        await updateOptions("micID");
      } else if (modelsContainer.value != "All") {
        await updateOptions("micModelName");
      } else {
        await updateOptions("micOEM");
      }
    } else if (
      transducerSelect.value == "Loudspeakers" &&
      deviceBoolContainer.checked
    ) {
      if (DeviceIDContainer.value != "All") {
        await updateOptions("speaker51ID");
      } else if (IDcontainers.value != "All") {
        await updateOptions("speakerID");
      } else if (modelsContainer.value != "All") {
        await updateOptions("speakerModelName");
      } else {
        await updateOptions("speakerOEM");
      }
    } else if (screenBoolContainer.checked) {
      if (aspectRatioContainer.value != "All") {
        await updateOptions("aspectRatio");
      } else {
        await updateOptions("screenPx");
      }
    }
  };

  document.getElementById("profilePlot").innerHTML =
    "<p style = 'font-size:20px'> Hit refresh or use selector to retrieve profiles <p>";
});
