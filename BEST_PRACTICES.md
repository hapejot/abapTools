# ABAP Best Practices - Quick Reference

## Error Handling

### ❌ Bad Practice
```abap
CALL FUNCTION 'SOME_FUNCTION'
  EXPORTING
    param = value
  EXCEPTIONS
    OTHERS = 0.  " Silently suppresses all errors
```

### ✅ Good Practice
```abap
CALL FUNCTION 'SOME_FUNCTION'
  EXPORTING
    param = value
  EXCEPTIONS
    specific_error_1 = 1
    specific_error_2 = 2
    OTHERS           = 3.
IF sy-subrc <> 0.
  " Handle error appropriately
ENDIF.
```

## Constants vs Magic Numbers

### ❌ Bad Practice
```abap
WHILE l_len > 255.
  APPEND l_str(255) TO r_result.
  SHIFT l_str LEFT BY 255 PLACES.
ENDWHILE.
```

### ✅ Good Practice
```abap
CONSTANTS:
  c_max_line_length TYPE i VALUE 255. " Maximum line length for SDYDO_TEXT_TABLE

WHILE l_len > c_max_line_length.
  APPEND l_str(c_max_line_length) TO r_result.
  SHIFT l_str LEFT BY c_max_line_length PLACES.
ENDWHILE.
```

## Code Comments

### ❌ Bad Practice
```abap
METHOD constructor.

ENDMETHOD.
```

### ✅ Good Practice
```abap
METHOD constructor.
  " Constructor is intentionally empty - initialization is handled in init() method
ENDMETHOD.
```

## HTML Best Practices

### ❌ Bad Practice
```html
<meta http-equiv="content-type" content="text/html; charset=utf-8"></meta>
```

### ✅ Good Practice
```html
<!DOCTYPE html>
<html>
<head>
  <meta http-equiv="content-type" content="text/html; charset=utf-8">
</head>
```

## Dead Code

### ❌ Bad Practice - Keep commented code
```abap
* APPEND 'var SimpleListModel = function(items) {' TO lt_js.
* APPEND '    this.items = ko.observableArray(items);' TO lt_js.
* APPEND '    this.itemToAdd = ko.observable("");' TO lt_js.
APPEND 'ko.applyBindings(g_Model);' TO lt_js.
```

### ✅ Good Practice - Remove or extract to separate file
```abap
APPEND 'ko.applyBindings(g_Model);' TO lt_js.
" For example code, see examples/knockout-patterns.js
```

## Debug Code

### ❌ Bad Practice
```abap
APPEND 'alert("binding completed");' TO lt_js.  " Debug alert in production
```

### ✅ Good Practice
```abap
" Note: Debug alert removed - enable in custom.js if needed for debugging
```

## Naming Conventions

### Standard ABAP Prefixes
- `l_` - Local variable
- `ls_` - Local structure
- `lt_` - Local table
- `lo_` - Local object reference
- `lr_` - Local data reference
- `lv_` - Local value (alternative to l_)
- `i_` - Importing parameter
- `e_` - Exporting parameter
- `c_` - Changing parameter
- `r_` - Returning parameter
- `g_` - Global variable (avoid when possible)
- `gt_` - Global table (avoid when possible)
- `c_` - Constant (e.g., c_max_length)

### Example
```abap
METHOD process_data.
  DATA: lt_results TYPE TABLE OF ty_result,
        ls_result  TYPE ty_result,
        lo_processor TYPE REF TO cl_processor,
        lv_count   TYPE i.
  
  CONSTANTS: c_max_items TYPE i VALUE 1000.
  
  " Implementation
ENDMETHOD.
```

## Documentation

### ✅ Good Practice
```abap
"! This class handles HTML/Knockout.js integration
"! It generates JavaScript data models from ABAP structures
CLASS zcl_html_kojs DEFINITION PUBLIC.
  
  "! Creates a new instance
  "! @parameter ir_parent | Parent GUI container
  "! @parameter r_result | Instance of the class
  CLASS-METHODS create
    IMPORTING
      ir_parent TYPE REF TO cl_gui_container
    RETURNING
      VALUE(r_result) TYPE REF TO zcl_html_kojs.
      
ENDCLASS.
```

## Code Organization

### Method Length
- Keep methods under 50 lines when possible
- If a method exceeds 100 lines, consider refactoring
- Each method should have a single, clear responsibility

### Class Responsibility
- Follow Single Responsibility Principle
- One class, one purpose
- Avoid "God Classes" that do everything

## Testing

### Unit Test Structure
```abap
CLASS ltc_test_class DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.
  
  PRIVATE SECTION.
    DATA: mo_cut TYPE REF TO zcl_html_kojs.
    
    METHODS: setup,
             test_string_to_table FOR TESTING,
             test_empty_string FOR TESTING.
ENDCLASS.

CLASS ltc_test_class IMPLEMENTATION.
  
  METHOD setup.
    " Initialize test fixture
  ENDMETHOD.
  
  METHOD test_string_to_table.
    " Arrange
    DATA(lv_input) = 'Test string'.
    
    " Act
    DATA(lt_result) = zcl_html_kojs=>string_to_table( lv_input ).
    
    " Assert
    cl_abap_unit_assert=>assert_not_initial( lt_result ).
  ENDMETHOD.
  
ENDCLASS.
```

## Security

### XSS Prevention
Always escape user input before inserting into HTML/JavaScript:

```abap
" Use appropriate escape format
APPEND |{ ls_comp-name } : '{ 
  escape( val = CONV string( l_outval ) 
          format = cl_abap_format=>e_html_js ) 
}'| TO gt_parts.
```

### Available Escape Formats
- `cl_abap_format=>e_html_text` - For HTML content
- `cl_abap_format=>e_html_js` - For JavaScript strings
- `cl_abap_format=>e_html_attr` - For HTML attributes
- `cl_abap_format=>e_json_string` - For JSON strings
- `cl_abap_format=>e_xml_text` - For XML content

## Performance

### Use Appropriate Collection Operations
```abap
" ✅ Good - Efficient table operation
READ TABLE lt_data INTO ls_data WITH KEY field = value.

" ❌ Avoid - Loop when direct read is possible
LOOP AT lt_data INTO ls_data WHERE field = value.
  EXIT.
ENDLOOP.
```

### Field Symbols for Large Structures
```abap
" ✅ Good - No copy overhead
LOOP AT lt_large_table ASSIGNING FIELD-SYMBOL(<fs_row>).
  <fs_row>-field = value.
ENDLOOP.

" ❌ Avoid - Copies entire structure
LOOP AT lt_large_table INTO DATA(ls_row).
  ls_row-field = value.
  MODIFY lt_large_table FROM ls_row.
ENDLOOP.
```
