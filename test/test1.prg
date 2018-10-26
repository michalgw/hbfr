#include "hbfr.ch"
#include "fileio.ch"

function main(cParam1, cParam2, cParam3, cParam4)
   LOCAL oFR1, oFR2, oFR3, oFR4, oFRC, hRow := hb_hash(), i, nt, ny, nu, f, cData
   AltD()
   ? "Loading library"
   IF !hbfr_LoadLibrary()
      ? "Can not load library."
      RETURN
   ENDIF

   ? "Loading report #1"
   USE TEST
   INDEX ON last + first TO testn
   // create report object
   oFR1 := TFreeReport():New()

   // Push data to report object
   DBGoTop()
   DO WHILE !Eof()
      hRow['first'] := AllTrim(test->first)
      hRow['last'] := AllTrim(test->last)
      hRow['street'] := AllTrim(test->street)
      hRow['city'] := AllTrim(test->city)
      hRow['state'] := AllTrim(test->state)
      hRow['zip'] := AllTrim(test->zip)
      hRow['hiredate'] := test->hiredate
      hRow['married'] := test->married
      hRow['age'] := test->age
      hRow['salary'] := test->salary
      hRow['notes'] := AllTrim(test->notes)
      // Add row to 'test' dataset from hash
      oFR1:AddRow('test', hRow)
      DBSkip()
   ENDDO
   // Register 'test' dataset
   oFR1:AddDataset('test')
   // Load report template from file
   oFR1:LoadFromFile('test1.frf')
   // Set report title
   oFR1:Title := 'Report #1'
   // Disable modal window, if enabled, program will wait until preview window is visible
   oFR1:ModalPreview := .F.
   // Do something after  close preview window
   oFR1:OnClosePreview := 'QOut("oFR1 closed")'
   // Show preview window
   oFR1:ShowReport()

   ? "Loading report #2"
   oFR2 := TFreeReport():New()
   // Add sample values to report
   FOR i := 0 TO 7
      oFR2:AddValue('test:' + AllTrim(Str(i)) + ':col', 'Value #' + AllTrim(Str(i)))
   NEXT
   // Load report from file and store in string
   f := FOpen('test2.frf')
   i := FSeek(f, 0, FS_END)
   FSeek(f, 0)
   cData := Space(i)
   FRead(f, @cData, i)
   FClose(f)
   // Load report from string
   oFR2:LoadFromMemory(cData)
   // Set report title
   oFR2:Title := 'Report #2'
   // Disable modal preview
   oFR2:ModalPreview := .F.
   // Do something after  close preview window
   oFR2:OnClosePreview := 'QOut("oFR2 closed")'
   // Show report preview
   oFR2:ShowReport()

   // Master-detail-subdetail - three datasets
   ? "Loading report #3"
   oFR3 := TFreeReport():New()
   FOR nt := 0 TO 10
      // Rows indexed from 0 !
      // Add master data - value name format 'dataset_name:row_no:column_name'
      // Access to master fields in report: [dataset_name:column_name]
      oFR3:AddValue('tab1:' + Str(nt) + ':col1', 'ABC' + Str(nt))
      oFR3:AddValue('tab1:' + Str(nt) + ':col2', nt)
      oFR3:AddValue('tab1:' + Str(nt) + ':col3', 0d20121201)
      FOR ny := 0 TO 5
         // Add detail data - value name format 'master_dataset:master_row_no:detail_dataset:detail_row_no:detail_column_name'
         // Access to detail fields in report: [masterdataset_name:detaildataset_name:detailcolumn_name]
         oFR3:AddValue('tab1:' + Str(nt) + ':tab2:' + Str(ny) + ':col1', 'CDE' + Str(ny))
         oFR3:AddValue('tab1:' + Str(nt) + ':tab2:' + Str(ny) + ':col2', 10 * nt + ny)
         FOR nu := 0 TO 5
            // Add subdetail data - value name format 'master_dataset:master_row_no:detail_dataset:detail_row_no:subdetail_dataset:sudetail_row_no:subdetaildetail_column_name'
            // Access to subdetail fields in report: [masterdataset_name:detaildataset_name:subdetaildataset_name:subdetailcolumn_name]
            oFR3:AddValue('tab1:' + Str(nt) + ':tab2:' + Str(ny) + ':tab3:' + Str(nu) + ':col1', 'FGH' + Str(nu))
            oFR3:AddValue('tab1:' + Str(nt) + ':tab2:' + Str(ny) + ':tab3:' + Str(nu) + ':col2', 100 * nt + 10 * ny + nu)
         NEXT
      NEXT
   NEXT
   // Register 'tab1' dataset - master data
   oFR3:AddDataset('tab1')
   // Register 'tab2' dataset - detail data
   oFR3:AddDataset('tab1:tab2')
   // Register 'tab3' dataset - subdetail data
   oFR3:AddDataset('tab1:tab2:tab3')
   // Load report file
   oFR3:LoadFromFile('test3.frf')
   oFR3:Title := 'Report #3'
   oFR3:ModalPreview := .F.
   oFR3:OnClosePreview := 'QOut("oFR3 closed")'
   oFR3:ShowReport()

   // FreeReport can evaluate harbour expression - just put harbour expression in the brackets [{}]
   // eg.: [{AllTrim(table->somecol}]
   // Only basic types are supported - string, memo, numeric, boolean, date, timestamp
   // HB expressions is evaluated via "Eval(&(cExpr))" on harbour side
   oFR4 := TFreeReport():New()
   // Register harbour dataset - params:
   // cDatasetName - name of dataset
   // cExprCheckEOF - harbor expression which returns a boolean value - .T. if is EOF
   // cExprFirst - harbour expression for jump to the first record
   // cExprNext - harbour expression for jump to the next record
   oFR4:AddHbDataset('data', 'Eof()', 'DbGoTop()', 'DbSkip()')
   oFR4:LoadFromFile('test4.frf')
   oFR4:Title := 'Report #4'
   oFR4:ModalPreview := .F.
   oFR4:OnClosePreview := 'QOut("oFR4 closed")'
   oFR4:ShowReport()

   ? "Loading composite report"
   // Create composite report
   oFRC := TFreeReport():New( .T. )
   // Add previously created report objects to composite report
   oFRC:AddReport(oFR1)
   oFRC:AddReport(oFR2)
   oFRC:AddReport(oFR3)
   oFRC:AddReport(oFR4)
   oFRC:DoublePass := .T.
   oFRC:Title := 'Test reports 1,2,3 and 4'
   oFRC:ModalPreview := .F.
   oFRC:OnClosePreview := 'QOut("oFRC closed")'
   oFRC:ShowReport()

   ? "Press ESC to exit"
   i := 0
   DO WHILE i <> 27
      i := Inkey(0)
      ? "Key pressed: " + Str(i)
   ENDDO

   oFR1 := NIL
   oFR2 := NIL
   oFR3 := NIL
   oFR4 := NIL
   oFRC := NIL