HTMLWidgets.widget({
  name: "handsontable",

  type: "output",

  factory: function (el, width, height) {
    let hot = null;

    // Function to calculate adaptive height based on row count
    function calculateAdaptiveHeight(config, availableHeight) {
      const doNotAdaptHeight =
        !config.data ||
        !Array.isArray(config.data) ||
        config.adaptiveHeight === false;

      if (doNotAdaptHeight) {
        return availableHeight;
      }

      const rowCount = config.data.length;
      const hasHeaders = config.colHeaders ? 1 : 0;
      const rowHeight = config.rowHeights || 30; // Default row height in pixels
      const headerHeight = 30; // Approximate header height in pixels

      // Calculate required height: header + (row count * row height)
      const requiredHeight = hasHeaders * headerHeight + rowCount * rowHeight;

      // If adaptiveHeight is set to true or not specified, adapt the height
      return Math.max(Math.min(requiredHeight, 1000), 100); // Min 100px, max 1000px
    }

    return {
      renderValue: function (x) {
        // Clear any existing instance
        if (hot) {
          hot.destroy();
          hot = null;
        }

        // Apply theme class to container
        if (x.themeName) {
          el.className = x.themeName;
        } else {
          el.className = "ht-theme-main-dark-auto";
        }

        // Set default configuration
        const config = Object.assign(
          {
            licenseKey: "non-commercial-and-evaluation",
            data: [],
            colHeaders: true,
            rowHeaders: true,
            width: width,
            height: height,
            autoWrapRow: true,
            autoWrapCol: true,
            copyPaste: true,
            fillHandle: true,
            manualColumnResize: true,
            manualRowResize: true,
            stretchH: "all",
            adaptiveHeight: true, // Default to true for adaptive height
          },
          x,
        );

        // Calculate adaptive height if enabled
        if (config.adaptiveHeight !== false) {
          config.height = calculateAdaptiveHeight(config, height);
        }

        // Handle data conversion if needed
        if (
          config.data &&
          Array.isArray(config.data) &&
          config.data.length > 0
        ) {
          // Ensure consistent row structure
          config.data = config.data.map((row) => {
            return Array.isArray(row) ? row : Object.values(row);
          });
        }

        // Create Handsontable instance
        try {
          hot = new Handsontable(el, config);

          // Store reference for Shiny
          if (HTMLWidgets.shinyMode) {
            el.hot = hot;

            // Set up change handlers for Shiny
            hot.addHook("afterChange", function (changes, source) {
              if (source !== "loadData" && changes) {
                Shiny.setInputValue(el.id + "_data", hot.getData(), {
                  priority: "event",
                });
                Shiny.setInputValue(el.id + "_changes", changes, {
                  priority: "event",
                });
              }
            });

            hot.addHook(
              "afterSelection",
              function (
                row,
                column,
                row2,
                column2,
                preventScrolling,
                selectionLayerLevel,
              ) {
                Shiny.setInputValue(
                  el.id + "_select",
                  {
                    row: row,
                    col: column,
                    row2: row2,
                    col2: column2,
                  },
                  { priority: "event" },
                );
              },
            );

            hot.addHook("afterCreateRow", function (index, amount, source) {
              if (source !== "loadData") {
                Shiny.setInputValue(el.id + "_data", hot.getData(), {
                  priority: "event",
                });
              }
            });

            hot.addHook(
              "afterRemoveRow",
              function (index, amount, physicalRows, source) {
                if (source !== "loadData") {
                  Shiny.setInputValue(el.id + "_data", hot.getData(), {
                    priority: "event",
                  });
                }
              },
            );

            hot.addHook("afterCreateCol", function (index, amount, source) {
              if (source !== "loadData") {
                Shiny.setInputValue(el.id + "_data", hot.getData(), {
                  priority: "event",
                });
              }
            });

            hot.addHook(
              "afterRemoveCol",
              function (index, amount, physicalColumns, source) {
                if (source !== "loadData") {
                  Shiny.setInputValue(el.id + "_data", hot.getData(), {
                    priority: "event",
                  });
                }
              },
            );
          }
        } catch (error) {
          console.error("Error creating Handsontable:", error);
          el.innerHTML =
            '<div style="color: red; padding: 20px;">Error: ' +
            error.message +
            "</div>";
        }
      },

      resize: function (width, height) {
        if (hot) {
          const config = hot.getSettings();
          let newHeight = height;

          // Apply adaptive height if enabled
          if (config.adaptiveHeight !== false) {
            newHeight = calculateAdaptiveHeight(config, height);
          }

          hot.updateSettings({
            width: width,
            height: newHeight,
          });
        }
      },

      getWidget: function () {
        return hot;
      },
    };
  },
});

// Utility function to convert Handsontable data to R format
if (HTMLWidgets.shinyMode) {
  Shiny.addCustomMessageHandler("handsontable-get-data", function (message) {
    const el = document.getElementById(message.id);
    if (el && el.hot) {
      const data = el.hot.getData();
      Shiny.setInputValue(message.id + "_data", data);
    }
  });

  Shiny.addCustomMessageHandler("handsontable-update-data", function (message) {
    const el = document.getElementById(message.id);
    if (el && el.hot) {
      el.hot.loadData(message.data);
    }
  });

  Shiny.addCustomMessageHandler(
    "handsontable-update-settings",
    function (message) {
      const el = document.getElementById(message.id);
      if (el && el.hot) {
        el.hot.updateSettings(message.settings);
      }
    },
  );
}
