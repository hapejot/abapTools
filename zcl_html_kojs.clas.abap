class ZCL_HTML_KOJS definition
  public
  final
  create private .

public section.

  types:
    tt_nametab TYPE TABLE OF fieldname WITH DEFAULT KEY .

  class-methods GET_GUI_COLOR
    importing
      !ID_ID type I default CL_WF_GUI_RESOURCES_4_HTML=>COL_BACKGROUND_LEVEL1
    returning
      value(RD_BGCOLOR) type STRING .
  class-methods CREATE
    importing
      !IR_PARENT type ref to CL_GUI_CONTAINER
    returning
      value(R_RESULT) type ref to ZCL_HTML_KOJS .
  class-methods PROJECT_TABLE
    importing
      !IT_TABLE type STANDARD TABLE
      !IT_NAMETAB type TT_NAMETAB
    returning
      value(RR_RESULT) type ref to DATA .
  class-methods PROJECT_STRUCT
    importing
      !IS_STRUCT type ANY
      !IT_NAMETAB type TT_NAMETAB
    returning
      value(RR_RESULT) type ref to DATA .
  class-methods STRING_TO_TABLE
    importing
      !I_STR type STRING optional
    returning
      value(R_RESULT) type SDYDO_TEXT_TABLE .
  class-methods HANDLE_DIALOG_CLOSE
    for event CLOSE of CL_GUI_DIALOGBOX_CONTAINER
    importing
      !SENDER .
  methods WRITE
    importing
      !I_NAME type STRING
      !I_DATA type ANY
    returning
      value(RR_OUTPUT) type ref to ZCL_HTML_KOJS .
  methods DISPLAY
    importing
      !IV_TEMPLATE type WWWDATAID-OBJID default 'ZORDERHEADER' .
  class-methods TEST .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA:
      gt_parts TYPE TABLE OF string .
    DATA m_value TYPE string .
    DATA mr_container TYPE REF TO cl_gui_container .
    DATA mr_viewer TYPE REF TO cl_gui_html_viewer .
    DATA:
      m_url TYPE c LENGTH 2000 .
    DATA m_sep TYPE string .
    DATA m_cmd TYPE string .

    METHODS init .
    METHODS constructor .
    METHODS write_data
      IMPORTING
        !i_data TYPE any .
    METHODS write_elem_object
      IMPORTING
        !io_type TYPE REF TO cl_abap_elemdescr
        !i_value TYPE data .
    METHODS write_struct_object
      IMPORTING
        !io_type  TYPE REF TO cl_abap_structdescr
        !i_struct TYPE data .
    METHODS write_table_object
      IMPORTING
        !io_type TYPE REF TO cl_abap_tabledescr
        !i_table TYPE table .
    METHODS install_files
      IMPORTING
        !iv_template TYPE wwwdataid-objid .
    METHODS generate_meta_header
      IMPORTING
        !io_struct_type TYPE REF TO cl_abap_structdescr
        !it_comps       TYPE cl_abap_structdescr=>component_table .
    METHODS handle_sapevent
          FOR EVENT sapevent OF cl_gui_html_viewer
      IMPORTING
          !action
          !frame
          !getdata
          !postdata
          !query_table .
ENDCLASS.



CLASS ZCL_HTML_KOJS IMPLEMENTATION.


  METHOD constructor.

  ENDMETHOD.


  METHOD create.
    CREATE OBJECT r_result.
    r_result->mr_container = ir_parent.
    r_result->init( ).
  ENDMETHOD.


  METHOD display.

    DATA l_html TYPE string.

    INSERT |g_Model = \{\n| INTO gt_parts INDEX 1.
    APPEND |\};\n| TO gt_parts.
    CONCATENATE LINES OF gt_parts INTO l_html.

    DATA(l_data) = string_to_table( l_html ).

    install_files( iv_template ).

    CALL METHOD mr_viewer->load_data
      EXPORTING
        url                    = 'data.js'
      CHANGING
        data_table             = l_data
      EXCEPTIONS
        dp_invalid_parameter   = 1
        dp_error_general       = 2
        cntl_error             = 3
        html_syntax_notcorrect = 4
        OTHERS                 = 5.

    CALL METHOD mr_viewer->show_url
      EXPORTING
        url                    = m_url
      EXCEPTIONS
        cntl_error             = 1
        cnht_error_not_allowed = 2
        cnht_error_parameter   = 3
        dp_error_general       = 4
        OTHERS                 = 5.

    CLEAR gt_parts[].

  ENDMETHOD.


  METHOD generate_meta_header.

    DATA lo_elem_descr TYPE REF TO cl_abap_elemdescr.
    DATA lo_str_descr   TYPE REF TO cl_abap_structdescr.
    DATA lo_descr      TYPE REF TO cl_abap_datadescr.
    DATA l_sep0        TYPE        string.
    DATA ls_comp       TYPE        abap_componentdescr.

    APPEND 'h: {' TO gt_parts.
    LOOP AT it_comps INTO      ls_comp  WHERE name IS NOT INITIAL.
      CALL METHOD io_struct_type->get_component_type
        EXPORTING
          p_name                 = ls_comp-name
        RECEIVING
          p_descr_ref            = lo_descr
        EXCEPTIONS
          component_not_found    = 1
          unsupported_input_type = 2
          OTHERS                 = 3.
      IF sy-subrc IS INITIAL.
        TRY.
            lo_elem_descr ?= lo_descr.
            DATA(l_fielddesc) = lo_elem_descr->get_ddic_field( sy-langu ).
            APPEND l_sep0 TO gt_parts.
            l_sep0 = ', '.
            APPEND |{ ls_comp-name }:\{width: { l_fielddesc-outputlen ALPHA = OUT
                        }, title: '{ escape( val = l_fielddesc-scrtext_s format = cl_abap_format=>e_html_js )
                        }', title_m: '{ escape( val = l_fielddesc-scrtext_m format = cl_abap_format=>e_html_js )
                        }', title_l: '{ escape( val = l_fielddesc-scrtext_l format = cl_abap_format=>e_html_js )
                        }'\}\n| TO gt_parts.
            APPEND || TO gt_parts.
            APPEND || TO gt_parts.
          CATCH cx_sy_move_cast_error.
*            lo_str_descr ?= lo_descr.
*            DATA(lt_comps) = lo_str_descr->get_components( ).
*            generate_meta_header( io_struct_type = lo_str_descr it_comps = lt_comps ).
        ENDTRY.

      ELSE.
        APPEND |// ERROR: { ls_comp-name }| TO gt_parts.
      ENDIF.
    ENDLOOP.
    APPEND '}' TO gt_parts.

  ENDMETHOD.


  METHOD get_gui_color.

    DATA: ld_back_color_i      TYPE i,
          ld_back_color_x      TYPE x LENGTH 3,
          ld_back_color_string TYPE string.

*Get default background color
    CALL METHOD cl_gui_resources=>get_background_color
      EXPORTING
        id     = id_id
        state  = 0
      IMPORTING
        color  = ld_back_color_i
      EXCEPTIONS
        OTHERS = 1.

*Convert color to hex value
    ld_back_color_x = ld_back_color_i.
    ld_back_color_string = ld_back_color_x.
    CONCATENATE '#' ld_back_color_string+4(2)
                    ld_back_color_string+2(2)
                    ld_back_color_string+0(2) INTO rd_bgcolor.
  ENDMETHOD.


  METHOD handle_dialog_close.
    DATA lr_dialog TYPE REF TO cl_gui_dialogbox_container.
    lr_dialog ?= sender.
    IF lr_dialog IS BOUND.
      lr_dialog->set_visible( abap_false ).
    ENDIF.
    LEAVE PROGRAM.
  ENDMETHOD.


  METHOD handle_sapevent.

    m_cmd = action.


    MESSAGE |got { getdata }.| TYPE 'S'.

  ENDMETHOD.


  METHOD init.

    CREATE OBJECT mr_viewer
      EXPORTING
        parent             = mr_container
      EXCEPTIONS
        cntl_error         = 1
        cntl_install_error = 2
        dp_install_error   = 3
        dp_error           = 4
        OTHERS             = 5.
    mr_viewer->set_registered_events(
      EXPORTING
        events                    = VALUE #( ( eventid = mr_viewer->m_id_sapevent ) ) ).
    SET HANDLER handle_sapevent FOR mr_viewer.
  ENDMETHOD.


  METHOD install_files.
    DATA lt_js TYPE TABLE OF tdline.

    CALL METHOD mr_viewer->load_mime_object
      EXPORTING
        object_id            = 'Z_KNOCKOUTJS'
        object_url           = 'knockoutjs.js'
      EXCEPTIONS
        object_not_found     = 1
        dp_invalid_parameter = 2
        dp_error_general     = 3
        OTHERS               = 4.

    CALL METHOD mr_viewer->load_mime_object
      EXPORTING
        object_id            = 'Z_W3CSS'
        object_url           = 'w3.css'
      EXCEPTIONS
        object_not_found     = 1
        dp_invalid_parameter = 2
        dp_error_general     = 3
        OTHERS               = 4.

    CALL METHOD mr_viewer->load_html_document
      EXPORTING
        document_id            = iv_template "'ZORDERHEADER'
        document_url           = 'main.html'
      IMPORTING
        assigned_url           = m_url
      EXCEPTIONS
        document_not_found     = 1
        dp_error_general       = 2
        dp_invalid_parameter   = 3
        html_syntax_notcorrect = 4
        OTHERS                 = 5.


*    APPEND 'var SimpleListModel = function(items) {                           ' TO lt_js.
*    APPEND '    this.items = ko.observableArray(items);                       ' TO lt_js.
*    APPEND '    this.itemToAdd = ko.observable("");                           ' TO lt_js.
*    APPEND '    this.addItem = function() {                                   ' TO lt_js.
*    APPEND '        if (this.itemToAdd() != "") {                             ' TO lt_js.
*    APPEND '            this.items.push(this.itemToAdd());                    ' TO lt_js.
*    APPEND '            this.itemToAdd("");                                   ' TO lt_js.
*    APPEND '        }                                                         ' TO lt_js.
*    APPEND '    }.bind(this);                                                 ' TO lt_js.
*    APPEND '                                                                  ' TO lt_js.
    APPEND 'ko.applyBindings(g_Model);' TO lt_js.
    APPEND 'alert("binding completed");                                                   ' TO lt_js.

    CALL METHOD mr_viewer->load_data
      EXPORTING
        url                    = 'custom.js'
      CHANGING
        data_table             = lt_js
      EXCEPTIONS
        dp_invalid_parameter   = 1
        dp_error_general       = 2
        cntl_error             = 3
        html_syntax_notcorrect = 4
        OTHERS                 = 5.


  ENDMETHOD.


  METHOD project_struct.

    DATA l_tab_desc         TYPE REF TO cl_abap_tabledescr.
    DATA l_rec_desc         TYPE REF TO cl_abap_structdescr.
    DATA l_new_rec_desc     TYPE REF TO cl_abap_structdescr.
    DATA l_typ_desc         TYPE REF TO cl_abap_typedescr.
    DATA l_fieldname        TYPE        fieldname.
    DATA lt_components      TYPE cl_abap_structdescr=>component_table.
    l_rec_desc ?= cl_abap_typedescr=>describe_by_data( is_struct ).
    LOOP AT l_rec_desc->get_components( ) INTO DATA(ls_comp).
      l_fieldname = VALUE #( it_nametab[ table_line = ls_comp-name ] OPTIONAL ).
      IF l_fieldname IS NOT INITIAL.
        APPEND INITIAL LINE TO lt_components REFERENCE INTO DATA(lr_comp).
        lr_comp->name = ls_comp-name.
        lr_comp->type = l_rec_desc->get_component_type( p_name = ls_comp-name ).
      ENDIF.
    ENDLOOP.
    l_new_rec_desc = cl_abap_structdescr=>create(
      EXPORTING
        p_components          = lt_components
    ).
    CREATE DATA rr_result TYPE HANDLE l_new_rec_desc.
    ASSIGN rr_result->* TO FIELD-SYMBOL(<fs_new_struct>).
    MOVE-CORRESPONDING is_struct TO <fs_new_struct>.

  ENDMETHOD.


  METHOD project_table.
    DATA l_tab_desc         TYPE REF TO cl_abap_tabledescr.
    DATA l_rec_desc         TYPE REF TO cl_abap_structdescr.
    DATA l_new_rec_desc     TYPE REF TO cl_abap_structdescr.
    DATA l_new_tab_desc     TYPE REF TO cl_abap_tabledescr.
    DATA l_typ_desc         TYPE REF TO cl_abap_typedescr.
    DATA l_fieldname        TYPE        fieldname.
    DATA lt_components      TYPE cl_abap_structdescr=>component_table.
    l_tab_desc ?= cl_abap_typedescr=>describe_by_data( p_data = it_table ).
    CHECK l_tab_desc IS BOUND.
    l_rec_desc ?= l_tab_desc->get_table_line_type( ).
    LOOP AT l_rec_desc->get_components( ) INTO DATA(ls_comp).
      l_fieldname = VALUE #( it_nametab[ table_line = ls_comp-name ] OPTIONAL ).
      IF l_fieldname IS NOT INITIAL.
        APPEND INITIAL LINE TO lt_components REFERENCE INTO DATA(lr_comp).
        lr_comp->name = ls_comp-name.
        lr_comp->type = l_rec_desc->get_component_type( p_name = ls_comp-name ).
      ENDIF.
    ENDLOOP.
    l_new_rec_desc = cl_abap_structdescr=>create(
      EXPORTING
        p_components          = lt_components
    ).
    l_new_tab_desc = cl_abap_tabledescr=>create(
                     p_line_type          = l_new_rec_desc
                 ).
    CREATE DATA rr_result TYPE HANDLE l_new_tab_desc.
    FIELD-SYMBOLS <fs_data> TYPE STANDARD TABLE.
    ASSIGN rr_result->* TO <fs_data> CASTING TYPE HANDLE l_new_tab_desc.
    LOOP AT it_table ASSIGNING FIELD-SYMBOL(<fs_src_row>).
      APPEND INITIAL LINE TO <fs_data> ASSIGNING FIELD-SYMBOL(<fs_dst_row>).
      MOVE-CORRESPONDING <fs_src_row> TO <fs_dst_row>.
    ENDLOOP.
  ENDMETHOD.


  METHOD string_to_table.

    DATA(l_str) = i_str.
    DATA(l_len) = strlen( l_str ).
    WHILE l_len > 255.
      APPEND l_str(255) TO r_result.
      SHIFT l_str LEFT BY 255 PLACES.
      l_len = strlen( l_str ).
    ENDWHILE.
    APPEND l_str TO r_result.

  ENDMETHOD.


  METHOD test.


    DATA(lr_html) = zcl_html_kojs=>create( cl_gui_container=>default_screen ).
    DATA(msg) = VALUE bapiret2( message = 'Meldung' ).
    lr_html->write(
            i_name = 'MSG'
            i_data = msg
          )->display( iv_template = 'Z_MAINJS' ).

  ENDMETHOD.


  METHOD write.

    DATA l_type TYPE c LENGTH 1.
    DATA l_comp TYPE i.

    APPEND |{ m_sep }{ i_name }:| TO gt_parts.
    m_sep = |,\n\n|.
    DESCRIBE FIELD i_data TYPE l_type COMPONENTS l_comp.
    CASE l_type.
      WHEN 'C'.
        APPEND escape( val = CONV string( i_data ) format = cl_abap_format=>e_xml_text ) TO gt_parts.

      WHEN OTHERS.
        write_data( i_data ).
    ENDCASE.
    rr_output = me.

  ENDMETHOD.


  METHOD write_data.
    FIELD-SYMBOLS <fs_data> TYPE any.

    ASSIGN i_data TO <fs_data>.
    DATA(lo_type) = cl_abap_typedescr=>describe_by_data( <fs_data> ).

    IF lo_type IS INSTANCE OF cl_abap_refdescr.
      ASSIGN i_data->* TO <fs_data>.
      lo_type = cl_abap_typedescr=>describe_by_data( <fs_data> ).
    ENDIF.

    CASE TYPE OF lo_type.
      WHEN TYPE cl_abap_elemdescr INTO DATA(lo_elem_type).
        write_elem_object(  io_type = lo_elem_type
                            i_value = <fs_data> ).
      WHEN TYPE cl_abap_structdescr INTO DATA(lo_struct_type).
        write_struct_object(  io_type = lo_struct_type
                              i_struct = <fs_data> ).
      WHEN TYPE cl_abap_tabledescr INTO DATA(lo_table_type).
        write_table_object(  io_type = lo_table_type
                              i_table = <fs_data> ).
    ENDCASE.

  ENDMETHOD.


  METHOD write_elem_object.

    DATA:
          l_string TYPE string.
    l_string = |'{ i_value }'|.
    APPEND escape( val = l_string format = cl_abap_format=>e_json_string ) TO gt_parts.

  ENDMETHOD.


  METHOD write_struct_object.

    DATA lo_elem_descr TYPE REF TO cl_abap_elemdescr.
    DATA l_outval      TYPE        c LENGTH 1000.
    DATA l_sep         TYPE        string.
    DATA(lt_comps) = io_type->get_components( ).
    FIELD-SYMBOLS <l_value> TYPE any.
    FIELD-SYMBOLS <fs_row>  TYPE any.

    APPEND |\{| TO gt_parts.
    generate_meta_header(
      io_struct_type = io_type
      it_comps       = lt_comps ).
    APPEND |, d:\{ | TO gt_parts.
    LOOP AT lt_comps INTO DATA(ls_comp).
      ASSIGN COMPONENT ls_comp-name OF STRUCTURE i_struct TO <l_value>.
*      DATA(ls_outval) = |{ <l_value> }|.
      DATA(lo_type) = cl_abap_typedescr=>describe_by_data( <l_value> ).
      CASE TYPE OF lo_type.
        WHEN TYPE cl_abap_elemdescr.
          WRITE <l_value> TO l_outval.
          IF l_outval IS NOT INITIAL.
            APPEND |{ l_sep }{ ls_comp-name } : '{ escape( val = CONV string( l_outval ) format = cl_abap_format=>e_html_js ) }'| TO gt_parts.
            l_sep = |, |.
          ENDIF.

        WHEN TYPE cl_abap_structdescr INTO DATA(lo_struct_type).
            APPEND |{ l_sep }{ ls_comp-name } : | TO gt_parts.
            l_sep = |, |.
          write_struct_object(  io_type = lo_struct_type
                                i_struct = <l_value> ).
        WHEN TYPE cl_abap_tabledescr INTO DATA(lo_table_type).
            APPEND |{ l_sep }{ ls_comp-name } : | TO gt_parts.
            l_sep = |, |.
          write_table_object(  io_type = lo_table_type
                                i_table = <l_value> ).
      ENDCASE.

    ENDLOOP.
    APPEND |\}\n\}| TO gt_parts.

  ENDMETHOD.


  METHOD write_table_object.

    DATA:
      lo_struct_type TYPE REF TO cl_abap_structdescr,
      lo_elem_descr  TYPE REF TO cl_abap_elemdescr,
      l_strvalue     TYPE c LENGTH 80,
      lo_descr       TYPE REF TO cl_abap_datadescr.

    DATA(lo_line_type) = io_type->get_table_line_type( ).
    DATA l_sep0 TYPE string.
    DATA l_sep1 TYPE string.
    FIELD-SYMBOLS <l_value> TYPE any.
    FIELD-SYMBOLS <fs_row> TYPE any.
    lo_struct_type ?= lo_line_type.
    CHECK lo_struct_type IS BOUND.
    DATA(lt_comps) = lo_struct_type->get_components( ).
    DATA ls_comp TYPE abap_componentdescr.

    APPEND |\{| TO gt_parts.
    generate_meta_header(
          io_struct_type = lo_struct_type
          it_comps       = lt_comps ).
    APPEND |,\n d: [| TO gt_parts.
    l_sep0 = ''.
    LOOP AT i_table ASSIGNING <fs_row>.
      APPEND l_sep0 TO gt_parts.
      l_sep0 = |,\n |.
      l_sep1 = ''.
      APPEND '{' TO gt_parts.
      LOOP AT lt_comps INTO ls_comp WHERE name IS NOT INITIAL.
        APPEND l_sep1 TO gt_parts.
        l_sep1 = |,\n  |.
        ASSIGN COMPONENT ls_comp-name OF STRUCTURE <fs_row> TO <l_value>.
        WRITE <l_value> TO l_strvalue.
        IF strlen( l_strvalue ) > 70.
          APPEND |{ ls_comp-name }:'{ escape( val = CONV string( <l_value> ) format = cl_abap_format=>e_html_js ) }'| TO gt_parts.
        ELSE.
          APPEND |{ ls_comp-name }:'{ escape( val = conv string( l_strvalue ) format = cl_abap_format=>e_html_js ) }'| TO gt_parts.
        ENDIF.
      ENDLOOP.
      APPEND '}' TO gt_parts.
    ENDLOOP.
    APPEND ']}' TO gt_parts.

  ENDMETHOD.
ENDCLASS.
