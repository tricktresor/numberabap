REPORT zt9r_numberabap NO STANDARD PAGE HEADING.

CLASS lcx_hit DEFINITION INHERITING FROM cx_static_check.
ENDCLASS.

CLASS lcx_no_hit DEFINITION INHERITING FROM cx_static_check.
ENDCLASS.

CLASS lcl_my_grid DEFINITION INHERITING FROM cl_gui_alv_grid.
   PUBLIC SECTION.
     METHODS set_no_resize.
ENDCLASS.

CLASS lcl_my_grid IMPLEMENTATION.
   METHOD set_no_resize.
     CALL METHOD me->set_resize_cols
       EXPORTING
         enable = 0
       EXCEPTIONS
         error  = 1
         OTHERS = 2.
   ENDMETHOD.
ENDCLASS.

CLASS lcl_main DEFINITION.
   PUBLIC SECTION.

     TYPES: ty_number TYPE c LENGTH 4,
            BEGIN OF ty_field,
              s01   TYPE ty_number,
              s02   TYPE ty_number,
              s03   TYPE ty_number,
              s04   TYPE ty_number,
              s05   TYPE ty_number,
              s06   TYPE ty_number,
              s07   TYPE ty_number,
              s08   TYPE ty_number,
              s09   TYPE ty_number,
              style TYPE lvc_t_styl,
            END OF ty_field,
            ty_field_all TYPE STANDARD TABLE OF ty_field,

            BEGIN OF ty_numbers,
              num TYPE n LENGTH 1,
              col TYPE i,
              row TYPE i,
            END OF ty_numbers,

            BEGIN OF ty_undo,
              field TYPE STANDARD TABLE OF ty_field WITH DEFAULT KEY,
              score TYPE i,
            END OF ty_undo
            .

     METHODS start IMPORTING type TYPE char01 sequence TYPE any.
     METHODS constructor.

   PROTECTED SECTION.
     DATA mt_field    TYPE STANDARD TABLE OF ty_field.
     DATA mt_undo     TYPE STANDARD TABLE OF ty_undo.
     DATA mv_type     TYPE c LENGTH 1.
     DATA mv_sequence TYPE n LENGTH 80.
     DATA mv_score    TYPE i.

     DATA mv_row      TYPE i.
     DATA mv_col      TYPE i.
     DATA c_row_max   TYPE i VALUE 9.

     DATA mv_last_clicked_row TYPE i.
     DATA mv_last_clicked_col TYPE i.

     DATA mv_first_click TYPE boolean.
     DATA mv_second_click TYPE boolean.

     DATA mr_grid TYPE REF TO lcl_my_grid.
     DATA mr_docker_game TYPE REF TO cl_gui_docking_container.
     DATA mr_docker_text TYPE REF TO cl_gui_docking_container.
     DATA mr_text TYPE REF TO cl_gui_textedit.

     METHODS display.
     METHODS init.
     METHODS refresh.
     METHODS delete_numbers IMPORTING row1 TYPE i
                                      col1 TYPE i
                                      row2 TYPE i
                                      col2 TYPE i.
     METHODS delete_number IMPORTING row TYPE i
                                     col TYPE i.
     METHODS check IMPORTING row1          TYPE i
                             col1          TYPE i
                             row2          TYPE i
                             col2          TYPE i
                   RETURNING VALUE(result) TYPE boolean.
     METHODS check_right
       IMPORTING row1 TYPE i
                 col1 TYPE i
                 row2 TYPE i
                 col2 TYPE i
       RAISING   lcx_hit
                 lcx_no_hit.
     METHODS check_down
       IMPORTING row1 TYPE i
                 col1 TYPE i
                 row2 TYPE i
                 col2 TYPE i
       RAISING   lcx_hit
                 lcx_no_hit.
     METHODS get_number
       IMPORTING row           TYPE i
                 col           TYPE i
                 field         TYPE ty_field_all OPTIONAL
       RETURNING VALUE(number) TYPE ty_number.
     METHODS check_hit
       IMPORTING num1 TYPE ty_number
                 num2 TYPE ty_number
       RAISING   lcx_hit
                 lcx_no_hit.
     METHODS check_solutions IMPORTING show TYPE boolean.
     METHODS add_number IMPORTING number TYPE clike.
     METHODS add_hotspot CHANGING field TYPE any.
     METHODS click IMPORTING i_row       TYPE any
                             i_fieldname TYPE clike.
     METHODS get_random_number RETURNING VALUE(number) TYPE numc1.
     METHODS add_end_button.
     METHODS add_numbers IMPORTING i_row TYPE i i_col TYPE i.
     METHODS shrink_lines.
     METHODS add_undo.
     METHODS undo.

*== button click event
     METHODS button_click FOR EVENT button_click OF lcl_my_grid
       IMPORTING es_col_id
                   es_row_no.
*== hotspot click event
     METHODS hotspot_click FOR EVENT hotspot_click OF lcl_my_grid
       IMPORTING e_row_id
                   e_column_id
                   es_row_no.
*== user command
     METHODS handle_user_command
       FOR EVENT user_command
                   OF cl_gui_alv_grid
       IMPORTING e_ucomm sender.
*== toolbar event
     METHODS handle_toolbar
       FOR EVENT toolbar
                   OF cl_gui_alv_grid
       IMPORTING e_object e_interactive sender.
*== Build toolbar
     METHODS build_toolbar
       IMPORTING i_object TYPE REF TO cl_alv_event_toolbar_set.

ENDCLASS.

CLASS lcl_main IMPLEMENTATION.
   METHOD constructor.
   ENDMETHOD.

   METHOD start.
     mv_type     = type.
     mv_sequence = sequence.
     SHIFT mv_sequence LEFT DELETING LEADING '0'.
     mv_col = 1.
     mv_row = 1.

     init( ).
     display( ).

   ENDMETHOD.


   METHOD display.

     IF mr_grid IS INITIAL.

       CREATE OBJECT mr_docker_game
         EXPORTING
           side  = cl_gui_docking_container=>dock_at_right
           ratio = 80.
       CREATE OBJECT mr_docker_text
         EXPORTING
           side      = cl_gui_docking_container=>dock_at_right
           extension = 2000. "Completely hide Selection Screen

       CREATE OBJECT mr_text
         EXPORTING
           parent = mr_docker_text.
       mr_text->set_statusbar_mode( 0 ).
       mr_text->set_toolbar_mode( 0 ).
       mr_text->set_readonly_mode( 1 ).


       CREATE OBJECT mr_grid
         EXPORTING
           i_parent = mr_docker_game.
       DATA ls_layout            TYPE lvc_s_layo.
       DATA lt_fieldcatalog      TYPE lvc_t_fcat.

       FIELD-SYMBOLS <fcat> LIKE LINE OF lt_fieldcatalog.
       DO 9 TIMES.
         APPEND INITIAL LINE TO lt_fieldcatalog ASSIGNING <fcat>.
         <fcat>-fieldname  = |S{ sy-index WIDTH = 2 ALIGN = RIGHT PAD = '0' } |.
         <fcat>-datatype   = 'NUMC'.
         <fcat>-outputlen  = '3'.
         <fcat>-just       = 'C'.
         <fcat>-f4availabl = abap_false.
       ENDDO.
       ls_layout-grid_title = 'Tentactics'.
       ls_layout-smalltitle = abap_true.
       ls_layout-no_rowmove = abap_true.
       ls_layout-no_headers = abap_true.
       ls_layout-stylefname = 'STYLE'.
       ls_layout-no_f4      = abap_true.

       DATA ls_exclude TYPE ui_func.
       DATA lt_exclude TYPE ui_functions.
       ls_exclude = cl_gui_alv_grid=>mc_fc_excl_all.
       APPEND ls_exclude TO lt_exclude.

       mr_grid->set_table_for_first_display(
         EXPORTING
           is_layout                     = ls_layout
           it_toolbar_excluding          = lt_exclude
         CHANGING
           it_outtab                     = mt_field
           it_fieldcatalog               = lt_fieldcatalog

         EXCEPTIONS
           OTHERS                        = 4 ).

       mr_grid->set_no_resize( ).
       SET HANDLER button_click        FOR mr_grid.
       SET HANDLER hotspot_click       FOR mr_grid.
       SET HANDLER handle_toolbar      FOR mr_grid.
       SET HANDLER handle_user_command FOR mr_grid.

       mr_grid->set_toolbar_interactive( ).
     ENDIF.

   ENDMETHOD.

   METHOD delete_numbers.

     delete_number( row = row1 col = col1 ).
     delete_number( row = row2 col = col2 ).
     SUBTRACT 5 FROM mv_score.
     refresh( ).

   ENDMETHOD.

   METHOD delete_number.

     DATA fieldname TYPE string.
     fieldname = |S{ col WIDTH = 2 ALIGN = RIGHT PAD = '0' }|.
     READ TABLE mt_field ASSIGNING FIELD-SYMBOL(<field>) INDEX row.
     ASSIGN COMPONENT fieldname OF STRUCTURE <field> TO FIELD-SYMBOL(<number>).
     IF sy-subrc = 0.
       CLEAR <number>.
     ENDIF.

     DATA ls_style TYPE lvc_s_styl.
     FIELD-SYMBOLS <style_tab> TYPE lvc_t_styl.

     ASSIGN COMPONENT 'STYLE' OF STRUCTURE <field> TO <style_tab>.
     IF sy-subrc = 0.
       READ TABLE <style_tab> ASSIGNING FIELD-SYMBOL(<style>)
             WITH KEY fieldname = fieldname.
       IF sy-subrc = 0.
         SUBTRACT cl_gui_alv_grid=>mc_style_button FROM <style>-style.
       ENDIF.
     ENDIF.

   ENDMETHOD.

   METHOD init.

     CASE mv_type.
       WHEN 'C'.
         DO.
           DATA(index) = sy-index - 1.
           DATA(number) = mv_sequence+index(1).
           IF number = space.
             EXIT.
           ELSE.
             add_number( number ).
           ENDIF.
         ENDDO.
       WHEN 'R'.
         DO 27 TIMES.
           add_number( get_random_number( ) ).
         ENDDO.
     ENDCASE.

     mv_score = 1000.
     add_end_button( ).
     add_undo( ).

   ENDMETHOD.

   METHOD add_end_button.

     FIELD-SYMBOLS <field> LIKE LINE OF mt_field.

     READ TABLE mt_field ASSIGNING <field> INDEX mv_row.
     IF sy-subrc > 0.
       APPEND INITIAL LINE TO mt_field ASSIGNING <field>.
     ENDIF.

     DATA(lv_fieldname) = |S{ mv_col WIDTH = 2 ALIGN = RIGHT PAD = '0' }|.
     ASSIGN COMPONENT lv_fieldname OF STRUCTURE <field> TO FIELD-SYMBOL(<position>).

     <position> = icon_next_object.

     add_hotspot( CHANGING field = <field> ) .


   ENDMETHOD.

   METHOD get_random_number.

     DATA random_number TYPE i.

     CALL FUNCTION 'QF05_RANDOM_INTEGER'
       EXPORTING
         ran_int_max = 9
         ran_int_min = 1
       IMPORTING
         ran_int     = random_number.

     number = random_number.

   ENDMETHOD.

   METHOD add_number.

     FIELD-SYMBOLS <field> LIKE LINE OF mt_field.

     CHECK number IS NOT INITIAL.

     READ TABLE mt_field ASSIGNING <field> INDEX mv_row.
     IF sy-subrc > 0.
       APPEND INITIAL LINE TO mt_field ASSIGNING <field>.
     ENDIF.

     DATA(lv_fieldname) = |S{ mv_col WIDTH = 2 ALIGN = RIGHT PAD = '0' }|.
     ASSIGN COMPONENT lv_fieldname OF STRUCTURE <field> TO FIELD-SYMBOL(<position>).

     <position> = number.

     add_hotspot( CHANGING field = <field> ) .

     ADD 1 TO mv_col.
     IF mv_col > c_row_max.
       mv_col = 1.
       ADD 1 TO mv_row.
     ENDIF.

   ENDMETHOD.


   METHOD add_hotspot.

     DATA ls_style TYPE lvc_s_styl.
     FIELD-SYMBOLS <style_tab> TYPE lvc_t_styl.

     ASSIGN COMPONENT 'STYLE' OF STRUCTURE field TO <style_tab>.
     CHECK sy-subrc = 0.
     DATA(lv_fieldname) = |S{ mv_col WIDTH = 2 ALIGN = RIGHT PAD = '0' }|.
     READ TABLE <style_tab> ASSIGNING FIELD-SYMBOL(<style>)
           WITH KEY fieldname = lv_fieldname.
     IF sy-subrc > 0.
       ls_style-fieldname = lv_fieldname.
       INSERT ls_style INTO TABLE <style_tab> ASSIGNING <style>.
     ENDIF.

     ADD cl_gui_alv_grid=>mc_style_button TO <style>-style.

   ENDMETHOD.

   METHOD button_click.

     click( i_row       = es_row_no-row_id
            i_fieldname = es_col_id-fieldname ).

   ENDMETHOD.

   METHOD hotspot_click.
     click( i_row       = es_row_no-row_id
            i_fieldname = e_column_id ).
   ENDMETHOD.

   METHOD click.

     DATA ls_color TYPE lvc_s_scol.
     FIELD-SYMBOLS <color_tab> TYPE lvc_t_scol.
     FIELD-SYMBOLS <field> LIKE LINE OF mt_field.
     FIELD-SYMBOLS <number> TYPE any.

     DATA row TYPE i.
     DATA col TYPE i.
     row = i_row.
     col = i_fieldname+1(2).

     CHECK mv_last_clicked_row <> row
        OR mv_last_clicked_col <> col.

     READ TABLE mt_field ASSIGNING <field> INDEX row.
     ASSIGN COMPONENT i_fieldname OF STRUCTURE <field> TO <number>.
     IF <number>(1) = '@'.
       add_numbers( i_row = row i_col = col ).
       RETURN.
     ENDIF.

     IF mv_first_click = abap_false AND
        mv_second_click = abap_false.
       mv_first_click = abap_true.
     ELSEIF mv_first_click = abap_true.
       mv_first_click = abap_false.
       mv_second_click = abap_true.
     ENDIF.


     IF mv_second_click = abap_true.

       IF check( row1 = mv_last_clicked_row
                 col1 = mv_last_clicked_col
                 row2 = row
                 col2 = col ) = abap_true.
*== pair found
         delete_numbers( row1 = row col1 = col
                         row2 = mv_last_clicked_row col2 = mv_last_clicked_col ).
         CLEAR mv_first_click.
         CLEAR mv_second_click.
         check_solutions( show = abap_false ).
         add_undo( ).
       ELSE.
*== not suitable

       ENDIF.
     ENDIF.

*== mark last click
     mv_last_clicked_row = row.
     mv_last_clicked_col = col.


   ENDMETHOD.

   METHOD add_numbers.

     DATA lt_numbers TYPE STANDARD TABLE OF i.
     DATA ls_number LIKE LINE OF lt_numbers.

     DATA lt_field TYPE ty_field_all.

     DATA row TYPE i.
     DATA col TYPE i.

     delete_number( row = i_row
                    col = i_col ).


     FIELD-SYMBOLS <field> LIKE LINE OF mt_field.
     DATA number TYPE ty_number.


     mv_row = i_row.
     mv_col = i_col.

     row = 0.
     col = 0.

     CLEAR number.
     lt_field = mt_field.

     WHILE number <> 'STOP'.
       ADD 1 TO row.
       col = 0.
       DO 9 TIMES.
         ADD 1 TO col.
         number = get_number( col = col row = row field = lt_field ).
         IF number = 'STOP'.
           EXIT. "from do
         ELSE.
           add_number( number ).
         ENDIF.
       ENDDO.
     ENDWHILE.


     add_end_button( ).
     check_solutions( show = abap_false ).
     refresh( ).

   ENDMETHOD.

   METHOD get_number.

     FIELD-SYMBOLS <field>  LIKE LINE OF mt_field.
     FIELD-SYMBOLS <number> TYPE ty_number.
     DATA fieldname         TYPE string.

     IF field IS SUPPLIED.
       READ TABLE field ASSIGNING <field> INDEX row.
     ELSE.
       READ TABLE mt_field ASSIGNING <field> INDEX row.
     ENDIF.

     IF sy-subrc > 0.
       number = 'STOP'.
     ELSE.
       fieldname = |S{ col WIDTH = 2 ALIGN = RIGHT PAD = '0' }|.
       ASSIGN COMPONENT fieldname OF STRUCTURE <field> TO <number>.
       number = <number>.
     ENDIF.

   ENDMETHOD.

   METHOD check.

     result = abap_false.

     TRY.

*== CHECK UP/ DOWN
         TRY.
             check_down( col1 = col1
                         row1 = row1
                         col2 = col2
                         row2 = row2 ).
           CATCH lcx_no_hit.
         ENDTRY.

*== CHECK LEFT/RIGHT
         TRY.
             check_right( col1 = col1
                          row1 = row1
                          col2 = col2
                          row2 = row2 ).
           CATCH lcx_no_hit.
         ENDTRY.


       CATCH lcx_hit.
         result = abap_true.
     ENDTRY.

   ENDMETHOD.

   METHOD check_right.

     DATA l_num1 TYPE ty_number. "Number of left click
     DATA l_num2 TYPE ty_number. "Number of right click
     DATA l_col  TYPE i.         "column current check
     DATA l_row  TYPE i.         "row current check
     DATA l_cnt  TYPE i.         "counter

     DATA l_row1 TYPE i.
     DATA l_row2 TYPE i.
     DATA l_col1 TYPE i.
     DATA l_col2 TYPE i.

*== get clicked cells in right order
     IF row1 <= row2.
       IF row1 = row2.
         IF col1 < col2.
           l_row1 = row1.
           l_row2 = row2.
           l_col1 = col1.
           l_col2 = col2.
         ELSE.
           l_row1 = row2.
           l_row2 = row1.
           l_col1 = col2.
           l_col2 = col1.
         ENDIF.
       ELSE.
         l_row1 = row1.
         l_row2 = row2.
         l_col1 = col1.
         l_col2 = col2.
       ENDIF.
     ELSE.
       l_row1 = row2.
       l_row2 = row1.
       l_col1 = col2.
       l_col2 = col1.
     ENDIF.




     l_col = l_col1.
     l_row = l_row1.

     IF row1 = row2.
*== Click in same row
       l_cnt = l_col2 - l_col1.
     ELSEIF l_row2 - l_row1 = 1.
*== Click in different rows
       l_cnt = 9 + l_col2 - l_col1.
     ELSE.
*== if more than 1 row difference: No hit possible
       RAISE EXCEPTION TYPE lcx_no_hit.
     ENDIF.

     l_col = l_col1.
     l_row = l_row1.

     DO l_cnt TIMES.

       ADD 1 TO l_col.
       IF l_col > 9.
         l_col = 1.
         ADD 1 TO l_row.
       ENDIF.

       l_num2 = get_number( col = l_col row = l_row ).
       IF l_col = l_col2 AND l_row = l_row2.
*== 2nd clicked cell reached
         "get top/left number
         l_num1 = get_number( col = l_col1 row = l_row1 ).
         "check if hit
         check_hit( num1 = l_num1 num2 = l_num2 ).

       ELSE.
         IF l_num2 IS NOT INITIAL.
           "path is not free
           RAISE EXCEPTION TYPE lcx_no_hit.
         ENDIF.
       ENDIF.
     ENDDO.


   ENDMETHOD.


   METHOD check_down.

     DATA l_num1 TYPE ty_number. "Number of top click
     DATA l_num2 TYPE ty_number. "Number of bottom click
     DATA l_row  TYPE i.         "row
     DATA l_cnt  TYPE i.         "counter

     DATA l_row1 TYPE i.
     DATA l_row2 TYPE i.
     DATA l_col1 TYPE i.
     DATA l_col2 TYPE i.

     IF col1 <> col2.
       "clicks must be in same column
       RAISE EXCEPTION TYPE lcx_no_hit.
     ENDIF.

     IF row1 = row2.
       "clicks must be in different rows
       RAISE EXCEPTION TYPE lcx_no_hit.
     ENDIF.

*== get clicked cells in right order
     IF row1 <= row2.
       l_row1 = row1.
       l_row2 = row2.
       l_col1 = col1.
       l_col2 = col2.
     ELSE.
       l_row1 = row2.
       l_row2 = row1.
       l_col1 = col2.
       l_col2 = col1.
     ENDIF.


     l_row = l_row1.
     l_cnt = l_row2 - l_row1.

     DO l_cnt TIMES.
       ADD 1 TO l_row.
*== get current cell
       l_num2 = get_number( col = l_col1 row = l_row ).

       IF l_row < l_row2.
*== check if path is clear
         IF l_num2 IS NOT INITIAL.
           "cell MUST be empty!
           RAISE EXCEPTION TYPE lcx_no_hit.
         ENDIF.
       ELSE.

*== reached 2nd clicked cell: Get number of first clicked cell
         l_num1 = get_number( col = l_col1 row = l_row1 ).

*== Check if hit
         check_hit( num1 = l_num1
                    num2 = l_num2 ).
       ENDIF.
     ENDDO.

   ENDMETHOD.

   METHOD check_hit.

     DATA n1 TYPE i.
     DATA n2 TYPE i.

     CHECK num1 CO '123456789 '.
     CHECK num2 CO '123456789 '.

     n1 = num1.
     n2 = num2.

     IF n1 + n2 = 10.
       RAISE EXCEPTION TYPE lcx_hit.
     ELSEIF n1 = n2.
       RAISE EXCEPTION TYPE lcx_hit.
     ELSE.
       RAISE EXCEPTION TYPE lcx_no_hit.
     ENDIF.

   ENDMETHOD.

   METHOD check_solutions.

     TYPES: BEGIN OF ty_check,
              num TYPE ty_number,
              col TYPE i,
              row TYPE i,
              sol TYPE boolean,
            END OF ty_check.

     DATA lt_numbers          TYPE STANDARD TABLE OF ty_check.
     DATA ls_number           TYPE ty_check.
     DATA ls_number2          TYPE ty_check.

     FIELD-SYMBOLS <number>   TYPE ty_check.
     FIELD-SYMBOLS <number2>  TYPE ty_check.

     DATA l_num1              TYPE ty_number.
     DATA l_num2              TYPE ty_number.
     DATA lv_index            TYPE i.

     DATA lv_num_of_solutions TYPE i.



*== build numbers table from playing field
     TRY.
         DO. "rows
           ADD 1 TO ls_number-row.
           ls_number-col = 0.
           DO 9 TIMES. "cols
             ADD 1 TO ls_number-col.
             ls_number-num = get_number( row = ls_number-row
                                         col = ls_number-col ).
             IF ls_number-num IS NOT INITIAL.
               IF ls_number-num(1) CN '123456789'. "NUM is CHAR4!
                 "Reached the end: @
                 RAISE EXCEPTION TYPE lcx_no_hit.
               ELSE.
                 "Add number to numbers table
                 APPEND ls_number TO lt_numbers.
               ENDIF.
             ENDIF.
           ENDDO.
         ENDDO.
       CATCH lcx_no_hit.
     ENDTRY.


*== check horizontal solutions
     LOOP AT lt_numbers ASSIGNING <number>.
       CHECK <number>-sol = abap_false.

       lv_index = sy-tabix.
       CHECK lv_index < lines( lt_numbers ).
       ADD 1 TO lv_index.
       READ TABLE lt_numbers ASSIGNING <number2> INDEX lv_index.
       TRY.
           check_hit( num1 = <number>-num
                      num2 = <number2>-num ).
         CATCH lcx_no_hit.
         CATCH lcx_hit.
           <number>-sol  = abap_true.
           <number2>-sol = abap_true.
           ADD 1 TO lv_num_of_solutions.
       ENDTRY.
     ENDLOOP.

*== Check vertical solutions
     SORT lt_numbers BY col row.

     LOOP AT lt_numbers ASSIGNING <number>.
       CHECK <number>-sol = abap_false.

       lv_index = sy-tabix.
       CHECK lv_index < lines( lt_numbers ).
       ADD 1 TO lv_index.

       READ TABLE lt_numbers ASSIGNING <number2> INDEX lv_index.
       CHECK <number>-col = <number2>-col.
       TRY.
           check_hit( num1 = <number>-num
                      num2 = <number2>-num ).
         CATCH lcx_no_hit.
         CATCH lcx_hit.
           <number>-sol  = abap_true.
           <number2>-sol = abap_true.
           ADD 1 TO lv_num_of_solutions.
       ENDTRY.
     ENDLOOP.

*== Build solutions
     IF show = abap_true.
       DATA l_text TYPE string.

       l_text =  |Solutions: { lv_num_of_solutions }{ cl_abap_char_utilities=>cr_lf }|.
       LOOP AT lt_numbers ASSIGNING <number> WHERE sol = abap_true.
         l_text = |{ l_text } { <number>-num }: row { <number>-row } col { <number>-col }{ cl_abap_char_utilities=>cr_lf }|.
       ENDLOOP.

       mr_text->set_textstream( l_text ).
     ENDIF.

   ENDMETHOD.


   METHOD refresh.

*== shrink lines before
     shrink_lines( ).

*== Refresh display
     DATA ls_stable TYPE lvc_s_stbl.
     ls_stable-col = abap_true.
     ls_stable-row = abap_true.

     mr_grid->refresh_table_display( is_stable = ls_stable i_soft_refresh = abap_true ).
     cl_gui_container=>set_focus( mr_text ).

   ENDMETHOD.

   METHOD shrink_lines.

     DATA lt_lines_to_delete TYPE STANDARD TABLE OF i.
     FIELD-SYMBOLS <field>   LIKE LINE OF mt_field.


*== shrink lines
     LOOP AT mt_field TRANSPORTING NO FIELDS
          WHERE s01 IS INITIAL
            AND s02 IS INITIAL
            AND s03 IS INITIAL
            AND s04 IS INITIAL
            AND s05 IS INITIAL
            AND s06 IS INITIAL
            AND s07 IS INITIAL
            AND s08 IS INITIAL
            AND s09 IS INITIAL.
       APPEND sy-index TO lt_lines_to_delete.
       ADD 10 TO mv_score.

     ENDLOOP.
     CHECK sy-subrc = 0.

     DELETE mt_field
          WHERE s01 IS INITIAL
            AND s02 IS INITIAL
            AND s03 IS INITIAL
            AND s04 IS INITIAL
            AND s05 IS INITIAL
            AND s06 IS INITIAL
            AND s07 IS INITIAL
            AND s08 IS INITIAL
            AND s09 IS INITIAL.

     IF mt_field IS INITIAL.
       MESSAGE i000(oo) WITH 'You won! Your Score:' mv_score.
     ELSEIF lines( mt_field ) = 1.
       READ TABLE mt_field ASSIGNING <field> INDEX 1.
       "Check, if last line is empty (except of "next-Button")
       "todo
     ENDIF.

   ENDMETHOD.

   METHOD add_undo.

     DATA ls_undo TYPE ty_undo.

     ls_undo-field = mt_field.
     ls_undo-score = mv_score.
     APPEND ls_undo TO mt_undo.

   ENDMETHOD.

   METHOD undo.

     DATA ls_undo TYPE ty_undo.

     CHECK lines( mt_undo ) > 1.

     DELETE mt_undo INDEX lines( mt_undo ).


     READ TABLE mt_undo INTO ls_undo INDEX lines( mt_undo ).
     CHECK sy-subrc = 0.

     mt_field = ls_undo-field.
     mv_score = ls_undo-score.
     refresh( ).

   ENDMETHOD.

   METHOD handle_user_command.
*** HandleUserCommand                  ***
*>>> Hierdurch wird die Methode "HANDLE_TOOLBAR" erneut durchlaufen

     CASE e_ucomm.
       WHEN 'Undo'.
         undo( ).
       WHEN 'Check'.
         check_solutions( show = abap_true ).
     ENDCASE.

*    mr_grid->set_toolbar_interactive( ).

   ENDMETHOD.                           "handle_user_command

   METHOD handle_toolbar.
*** HandleToolbar                      ***
     build_toolbar( e_object ).

   ENDMETHOD.                    "handle_toolbar

   METHOD build_toolbar.
*== build Toolbar
     DATA: ls_toolbar  TYPE stb_button.

*== Icon "Undo"
     CLEAR ls_toolbar.
     MOVE 'Undo'                   TO ls_toolbar-function.
     MOVE 'Undo last move'         TO ls_toolbar-quickinfo.
     MOVE 'Undo'                   TO ls_toolbar-text.
     MOVE icon_system_undo         TO ls_toolbar-icon.
     APPEND ls_toolbar TO i_object->mt_toolbar.

*== Icon "Check Solutions"
     CLEAR ls_toolbar.
     MOVE 'Check'                  TO ls_toolbar-function.
     MOVE 'Check solutions'        TO ls_toolbar-quickinfo.
     MOVE 'Check'                  TO ls_toolbar-text.
     MOVE icon_check               TO ls_toolbar-icon.
     APPEND ls_toolbar TO i_object->mt_toolbar.

*== Icon "Score"
     CLEAR ls_toolbar.
     MOVE 'Score'                  TO ls_toolbar-function.
     MOVE 'Current Score'          TO ls_toolbar-quickinfo.
     ls_toolbar-text = |Score:{ mv_score }|.
     MOVE icon_modify              TO ls_toolbar-icon.
     MOVE abap_true                TO ls_toolbar-checked.
     MOVE abap_true                TO ls_toolbar-disabled.
     APPEND ls_toolbar TO i_object->mt_toolbar.


   ENDMETHOD.                    "build_toolbar


ENDCLASS.


DATA gr_main TYPE REF TO lcl_main.


PARAMETERS p_typ AS LISTBOX VISIBLE LENGTH 40 OBLIGATORY DEFAULT 'C' USER-COMMAND enter.
PARAMETERS p_seq TYPE n LENGTH 80 DEFAULT '123456789111213141516171819' MODIF ID dsp .

AT SELECTION-SCREEN OUTPUT.
   LOOP AT SCREEN.
     IF screen-group1 = 'DSP'.
       CASE p_typ.
         WHEN 'C'.
           screen-active = '1'.
         WHEN 'R'.
           screen-active = '0'.
       ENDCASE.
       MODIFY SCREEN.
     ENDIF.
   ENDLOOP.


INITIALIZATION.

   DATA(values) = VALUE vrm_values( ( key = 'C' text = 'Classic' )
                                    ( key = 'R' text = 'Random' ) ).

   CALL FUNCTION 'VRM_SET_VALUES'
     EXPORTING
       id     = 'P_TYP'
       values = values
     EXCEPTIONS
       OTHERS = 2.

AT LINE-SELECTION.

   DATA field_name TYPE string.
   DATA field_value TYPE string.
   DATA field_row TYPE i.
   DATA field_col TYPE i.
   GET CURSOR FIELD field_name VALUE field_value LINE field_row OFFSET field_col.


   BREAK-POINT.

AT SELECTION-SCREEN.

   WRITE space.
   CHECK gr_main IS INITIAL.
   gr_main = NEW lcl_main( ).
   gr_main->start( type = p_typ sequence = p_seq ).
