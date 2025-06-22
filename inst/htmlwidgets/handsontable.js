
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

        // Add CSV export functionality to context menu
        if (config.contextMenu === true) {
          config.contextMenu = {
            items: {
              "row_above": {},
              "row_below": {},
              "col_left": {},
              "col_right": {},
              "remove_row": {},
              "remove_col": {},
              "separator1": "---------",
              "copy": {},
              "cut": {},
              "separator2": "---------",
              "export_csv": {
                name: "Download to CSV",
                callback: function() {
                  // CSV export that handles empty values (fixes #434)
                  const hot = this;
                  const data = hot.getData();
                  const settings = hot.getSettings();
                  const colHeaders = settings.colHeaders;
                  
                  let csvContent = '';
                  
                  // Add headers if they exist and are not just true/false
                  if (colHeaders && Array.isArray(colHeaders) && colHeaders.length > 0) {
                    csvContent += colHeaders.map(header => {
                      // Handle null/undefined headers
                      const value = header == null ? '' : String(header);
                      // Escape quotes and wrap in quotes if contains comma, quote, or newline
                      return value.indexOf(',') !== -1 || value.indexOf('"') !== -1 || value.indexOf('\n') !== -1 
                        ? '"' + value.replace(/"/g, '""') + '"' 
                        : value;
                    }).join(',') + '\n';
                  }
                  
                  // Add data rows
                  data.forEach(row => {
                    if (row) {
                      const csvRow = row.map(cell => {
                        // Handle null/undefined/empty values (key fix for issue #434)
                        const value = cell == null ? '' : String(cell);
                        // Escape quotes and wrap in quotes if contains comma, quote, or newline
                        return value.indexOf(',') !== -1 || value.indexOf('"') !== -1 || value.indexOf('\n') !== -1 
                          ? '"' + value.replace(/"/g, '""') + '"' 
                          : value;
                      }).join(',');
                      csvContent += csvRow + '\n';
                    }
                  });
                  
                  // Create and download file
                  const blob = new Blob([csvContent], { type: 'text/csv;charset=utf-8;' });
                  const link = document.createElement('a');
                  
                  if (link.download !== undefined) {
                    const url = URL.createObjectURL(blob);
                    link.setAttribute('href', url);
                    link.setAttribute('download', 'handsontable_export.csv');
                    link.style.visibility = 'hidden';
                    document.body.appendChild(link);
                    link.click();
                    document.body.removeChild(link);
                  }
                }
              }
            }
          };
        }

        // Transform context menu customOpts to Handsontable v6.2.2 format
        if (
          config.contextMenu &&
          typeof config.contextMenu === "object" &&
          config.contextMenu.customOpts
        ) {
          // Start with default items including CSV export
          let defaultItems = {};
          
          if (config.contextMenu.items) {
            // If items already exist, use them as base
            defaultItems = { ...config.contextMenu.items };
          } else {
            // Create default items with CSV export
            defaultItems = {
              "row_above": {},
              "row_below": {},
              "col_left": {},
              "col_right": {},
              "remove_row": {},
              "remove_col": {},
              "separator1": "---------",
              "copy": {},
              "cut": {},
              "separator2": "---------",
              "export_csv": {
                name: "Download to CSV",
                callback: function() {
                  // Same CSV export function as above
                  const hot = this;
                  const data = hot.getData();
                  const settings = hot.getSettings();
                  const colHeaders = settings.colHeaders;
                  
                  let csvContent = '';
                  
                  if (colHeaders && Array.isArray(colHeaders) && colHeaders.length > 0) {
                    csvContent += colHeaders.map(header => {
                      const value = header == null ? '' : String(header);
                      return value.indexOf(',') !== -1 || value.indexOf('"') !== -1 || value.indexOf('\n') !== -1 
                        ? '"' + value.replace(/"/g, '""') + '"' 
                        : value;
                    }).join(',') + '\n';
                  }
                  
                  data.forEach(row => {
                    if (row) {
                      const csvRow = row.map(cell => {
                        const value = cell == null ? '' : String(cell);
                        return value.indexOf(',') !== -1 || value.indexOf('"') !== -1 || value.indexOf('\n') !== -1 
                          ? '"' + value.replace(/"/g, '""') + '"' 
                          : value;
                      }).join(',');
                      csvContent += csvRow + '\n';
                    }
                  });
                  
                  const blob = new Blob([csvContent], { type: 'text/csv;charset=utf-8;' });
                  const link = document.createElement('a');
                  
                  if (link.download !== undefined) {
                    const url = URL.createObjectURL(blob);
                    link.setAttribute('href', url);
                    link.setAttribute('download', 'handsontable_export.csv');
                    link.style.visibility = 'hidden';
                    document.body.appendChild(link);
                    link.click();
                    document.body.removeChild(link);
                  }
                }
              }
            };
          }

          const transformedItems = { ...defaultItems };

          for (const [key, value] of Object.entries(
            config.contextMenu.customOpts,
          )) {
            if (value === true) {
              // For boolean true, use the key as a built-in action
              transformedItems[key] = {};
            } else if (value === false) {
              // Skip false values (disabled items) - this allows users to disable CSV export
              if (key === "export_csv") {
                delete transformedItems[key];
              }
              continue;
            } else {
              // Use the value as-is for objects with name/callback, etc.
              transformedItems[key] = value;
            }
          }

          config.contextMenu = { items: transformedItems };
          delete config.contextMenu.customOpts;
        }

        // Transform R configuration to Handsontable v6.2.2 format
        if (config.columns && Array.isArray(config.columns)) {
          config.columns = config.columns.map((col) => {
            if (!col) return col;

            const transformedCol = { ...col };

            // Transform validator objects
            if (col.validator && typeof col.validator === "object") {
              const validator = col.validator;

              // Set validator to string alias or function
              if (validator.type === "numeric") {
                function isNumeric(str) {
                  return (
                    !Number.isNaN(Number(str)) && !Number.isNaN(parseFloat(str))
                  );
                }

                // Create custom numeric validator with min/max constraints
                transformedCol.validator = function (value, callback) {
                  // Allow empty values
                  if (value === null || value === undefined || value === "") {
                    callback(true);
                    return;
                  }

                  // Check if it's a valid number
                  if (!isNumeric(value)) {
                    callback(false);
                    return;
                  }

                  // Convert to number
                  const numValue = Number(value);

                  // Check min constraint
                  if (validator.min !== undefined && numValue < validator.min) {
                    callback(false);
                    return;
                  }

                  // Check max constraint
                  if (validator.max !== undefined && numValue > validator.max) {
                    callback(false);
                    return;
                  }

                  callback(true);
                };
              } else if (validator.type === "list") {
                transformedCol.validator = "dropdown";
                if (validator.source !== undefined) {
                  transformedCol.source = validator.source;
                }
              } else if (validator.type === "regexp") {
                // Create custom regexp validator function
                if (validator.pattern) {
                  const pattern = new RegExp(validator.pattern);
                  transformedCol.validator = function (value, callback) {
                    if (value === null || value === undefined || value === "") {
                      callback(true); // Allow empty values
                    } else {
                      callback(pattern.test(String(value)));
                    }
                  };
                }
              }

              // Handle allowInvalid at column level
              if (validator.allowInvalid !== undefined) {
                transformedCol.allowInvalid = validator.allowInvalid;
              }

              // Remove the original validator object properties
              delete transformedCol.validator.type;
              delete transformedCol.validator.min;
              delete transformedCol.validator.max;
              delete transformedCol.validator.source;
              delete transformedCol.validator.pattern;
              delete transformedCol.validator.allowInvalid;
              delete transformedCol.validator.strict;
            }

            // Transform numeric formatting
            if (col.numericFormat && typeof col.numericFormat === "object") {
              transformedCol.numericFormat = col.numericFormat;
            }

            // Transform date picker configuration
            if (
              col.datePickerConfig &&
              typeof col.datePickerConfig === "object"
            ) {
              transformedCol.datePickerConfig = col.datePickerConfig;
            }

            // Handle cell type specific transformations
            if (col.type) {
              switch (col.type) {
                case "autocomplete":
                  // Ensure autocomplete has proper configuration
                  if (col.source && !transformedCol.source) {
                    transformedCol.source = col.source;
                  }
                  break;
                case "password":
                  // Password type configuration
                  if (col.copyable !== undefined) {
                    transformedCol.copyable = col.copyable;
                  }
                  break;
                case "numeric":
                  // Handle numeric format options
                  if (col.numericFormat) {
                    transformedCol.numericFormat = col.numericFormat;
                  }
                  break;
                case "date":
                  // Handle date configuration
                  if (col.dateFormat) {
                    transformedCol.dateFormat = col.dateFormat;
                  }
                  if (col.datePickerConfig) {
                    transformedCol.datePickerConfig = col.datePickerConfig;
                  }
                  break;
              }
            }

            return transformedCol;
          });
        }

        // Create Handsontable instance
        try {
          hot = new Handsontable(el, config);
          
          // Initialize dynamic column names tracking
          let currentColnames = config.originalColnames ? [...config.originalColnames] : [];

          // Column name tracking - no special default value handling needed

          // Store reference for Shiny
          if (HTMLWidgets.shinyMode) {
            el.hot = hot;

            // Set up change handlers for Shiny
            hot.addHook("afterChange", function (changes, source) {
              if (source !== "loadData" && changes) {
                const data = hot.getData();
                // Transform changes to named objects with 1-indexed values
                const transformedChanges = changes.map(change => ({
                  row_idx: change[0] + 1,  // Convert to 1-indexed
                  col_idx: change[1] + 1,  // Convert to 1-indexed
                  old_val: change[2],
                  new_val: change[3]
                }));
                // Send to input$table_id (main input) with current column names
                Shiny.setInputValue(
                  el.id,
                  {
                    data: data,
                    event: "afterChange",
                    changes: transformedChanges,
                    source: source,
                    colnames: currentColnames, // Use dynamic column names
                  },
                  {
                    priority: "event",
                  },
                );
                // Also send to separate inputs for compatibility
                Shiny.setInputValue(el.id + "_data", data, {
                  priority: "event",
                });
                Shiny.setInputValue(el.id + "_changes", transformedChanges, {
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
                    row_idx: row + 1,      // Convert to 1-indexed
                    col_idx: column + 1,   // Convert to 1-indexed
                    row2_idx: row2 + 1,    // Convert to 1-indexed
                    col2_idx: column2 + 1, // Convert to 1-indexed
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
                    index: index + 1,  // Convert to 1-indexed
                    amount: amount,
                    colnames: currentColnames, // Use dynamic column names
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
                      index: index + 1,  // Convert to 1-indexed
                      amount: amount,
                      colnames: currentColnames, // Use dynamic column names
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
                // Update column names array for added columns
                for (let i = 0; i < amount; i++) {
                  const newColName = "col_" + (currentColnames.length + i + 1);
                  currentColnames.splice(index + i, 0, newColName);
                }
                
                const data = hot.getData();
                Shiny.setInputValue(
                  el.id,
                  {
                    data: data,
                    event: "afterCreateCol",
                    index: index + 1,  // Convert to 1-indexed
                    amount: amount,
                    colnames: currentColnames, // Updated column names
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
                  // Update column names array for removed columns
                  currentColnames.splice(index, amount);
                  
                  const data = hot.getData();
                  Shiny.setInputValue(
                    el.id,
                    {
                      data: data,
                      event: "afterRemoveCol",
                      index: index + 1,  // Convert to 1-indexed
                      amount: amount,
                      colnames: currentColnames, // Updated column names
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
      if (
        Array.isArray(message.row) &&
        Array.isArray(message.col) &&
        Array.isArray(message.value)
      ) {
        // Multiple cell updates - use setDataAtRowProp for efficiency
        const changes = [];
        const minLength = Math.min(
          message.row.length,
          message.col.length,
          message.value.length,
        );

        for (let i = 0; i < minLength; i++) {
          changes.push([message.row[i], message.col[i], message.value[i]]);
        }

        // Use setDataAtRowProp for batch updates
        hot.setDataAtRowProp(changes);
      } else {
        // Single cell update - convert arrays to single values if needed
        const row = Array.isArray(message.row) ? message.row[0] : message.row;
        const col = Array.isArray(message.col) ? message.col[0] : message.col;
        const value = Array.isArray(message.value)
          ? message.value[0]
          : message.value;

        hot.setDataAtCell(row, col, value);
      }
    }
  });
}
