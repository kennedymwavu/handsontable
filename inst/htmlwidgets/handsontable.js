HTMLWidgets.widget({
  name: "handsontable",

  type: "output",

  factory: function (el, width, height) {
    let hot = null;

    // Function to calculate adaptive height based on row count and row heights
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
      const headerHeight = 30; // Approximate header height in pixels
      let totalRowHeight = 0;

      // Handle different types of rowHeights configuration as per Handsontable docs
      if (typeof config.rowHeights === "number") {
        // If rowHeights is a single number, apply to all rows
        totalRowHeight = rowCount * config.rowHeights;
      } else if (Array.isArray(config.rowHeights)) {
        // If rowHeights is an array, sum up specified heights and use default for the rest
        const defaultRowHeight = 30; // Default row height in pixels
        for (let i = 0; i < rowCount; i++) {
          if (i < config.rowHeights.length) {
            totalRowHeight += config.rowHeights[i] || defaultRowHeight;
          } else {
            totalRowHeight += defaultRowHeight;
          }
        }
      } else if (typeof config.rowHeights === "function") {
        // If rowHeights is a function, we can't calculate exactly without rendering
        // So we use an approximation based on default height
        totalRowHeight = rowCount * 30;
      } else {
        // Default case: use standard row height for all rows
        const defaultRowHeight = 30;
        totalRowHeight = rowCount * defaultRowHeight;
      }

      // Calculate required height: header + total row height
      const requiredHeight = hasHeaders * headerHeight + totalRowHeight;

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
                const data = hot.getData();
                // Send to input$table_id (main input)
                Shiny.setInputValue(
                  el.id,
                  {
                    data: data,
                    changes: changes,
                  },
                  {
                    priority: "event",
                  },
                );
                // Also send to separate inputs for compatibility
                Shiny.setInputValue(el.id + "_data", data, {
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
                const data = hot.getData();
                Shiny.setInputValue(
                  el.id,
                  {
                    data: data,
                    event: "afterCreateRow",
                    index: index,
                    amount: amount,
                  },
                  {
                    priority: "event",
                  },
                );
                Shiny.setInputValue(el.id + "_data", data, {
                  priority: "event",
                });
              }
            });

            hot.addHook(
              "afterRemoveRow",
              function (index, amount, physicalRows, source) {
                if (source !== "loadData") {
                  const data = hot.getData();
                  Shiny.setInputValue(
                    el.id,
                    {
                      data: data,
                      event: "afterRemoveRow",
                      index: index,
                      amount: amount,
                    },
                    {
                      priority: "event",
                    },
                  );
                  Shiny.setInputValue(el.id + "_data", data, {
                    priority: "event",
                  });
                }
              },
            );

            hot.addHook("afterCreateCol", function (index, amount, source) {
              if (source !== "loadData") {
                const data = hot.getData();
                Shiny.setInputValue(
                  el.id,
                  {
                    data: data,
                    event: "afterCreateCol",
                    index: index,
                    amount: amount,
                  },
                  {
                    priority: "event",
                  },
                );
                Shiny.setInputValue(el.id + "_data", data, {
                  priority: "event",
                });
              }
            });

            hot.addHook(
              "afterRemoveCol",
              function (index, amount, physicalColumns, source) {
                if (source !== "loadData") {
                  const data = hot.getData();
                  Shiny.setInputValue(
                    el.id,
                    {
                      data: data,
                      event: "afterRemoveCol",
                      index: index,
                      amount: amount,
                    },
                    {
                      priority: "event",
                    },
                  );
                  Shiny.setInputValue(el.id + "_data", data, {
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
    const $el = $("#" + message.id);
    if ($el.length && $el[0].hot) {
      const data = $el[0].hot.getData();
      Shiny.setInputValue(message.id + "_data", data);
    }
  });

  Shiny.addCustomMessageHandler("handsontable-update-data", function (message) {
    const $el = $("#" + message.id);
    if ($el.length && $el[0].hot) {
      $el[0].hot.loadData(message.data);
    }
  });

  Shiny.addCustomMessageHandler(
    "handsontable-update-settings",
    function (message) {
      const $el = $("#" + message.id);
      if ($el.length && $el[0].hot) {
        $el[0].hot.updateSettings(message.settings);
      }
    },
  );

  Shiny.addCustomMessageHandler("handsontable-set-data", function (message) {
    const $el = $("#" + message.id);
    if ($el.length && $el[0].hot) {
      const hot = $el[0].hot;
      
      // Handle vectorized data from R
      if (Array.isArray(message.row) && Array.isArray(message.col) && Array.isArray(message.value)) {
        // Multiple cell updates - use setDataAtRowProp for efficiency
        const changes = [];
        const minLength = Math.min(message.row.length, message.col.length, message.value.length);
        
        for (let i = 0; i < minLength; i++) {
          changes.push([message.row[i], message.col[i], message.value[i]]);
        }
        
        // Use setDataAtRowProp for batch updates
        hot.setDataAtRowProp(changes);
      } else {
        // Single cell update - convert arrays to single values if needed
        const row = Array.isArray(message.row) ? message.row[0] : message.row;
        const col = Array.isArray(message.col) ? message.col[0] : message.col;
        const value = Array.isArray(message.value) ? message.value[0] : message.value;
        
        hot.setDataAtCell(row, col, value);
      }
    }
  });
}
