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
   oFR1 := TFreeReport():New()
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
      oFR1:AddRow('test', hRow)
      DBSkip()
   ENDDO
   oFR1:AddDataset('test')
   oFR1:LoadFromFile('test1.frf')
   oFR1:Title := 'Report #1'
   oFR1:ModalPreview := .F.
   oFR1:ShowReport()

   ? "Loading report #2"
   oFR2 := TFreeReport():New()
   FOR i := 0 TO 7
      oFR2:AddValue('test:' + AllTrim(Str(i)) + ':col', 'Value #' + AllTrim(Str(i)))
   NEXT
   f := FOpen('test2.frf')
   i := FSeek(f, 0, FS_END)
   FSeek(f, 0)
   cData := Space(i)
   FRead(f, @cData, i)
   FClose(f)
   oFR2:LoadFromMemory(cData)
   oFR2:Title := 'Report #2'
   oFR2:ModalPreview := .F.
   oFR2:ShowReport()

   ? "Loading report #3"
   oFR3 := TFreeReport():New()
   FOR nt := 0 TO 10
      oFR3:AddValue('tab1:' + Str(nt) + ':col1', 'ABC' + Str(nt))
      oFR3:AddValue('tab1:' + Str(nt) + ':col2', nt)
      oFR3:AddValue('tab1:' + Str(nt) + ':col3', 0d20121201)
      FOR ny := 0 TO 5
         oFR3:AddValue('tab1:' + Str(nt) + ':tab2:' + Str(ny) + ':col1', 'CDE' + Str(ny))
         oFR3:AddValue('tab1:' + Str(nt) + ':tab2:' + Str(ny) + ':col2', 10 * nt + ny)
         FOR nu := 0 TO 5
            oFR3:AddValue('tab1:' + Str(nt) + ':tab2:' + Str(ny) + ':tab3:' + Str(nu) + ':col1', 'CDE' + Str(nu))
            oFR3:AddValue('tab1:' + Str(nt) + ':tab2:' + Str(ny) + ':tab3:' + Str(nu) + ':col2', 100 * nt + 10 * ny + nu)
         NEXT
      NEXT
   NEXT
   oFR3:AddDataset('tab1')
   oFR3:AddDataset('tab1:tab2')
   oFR3:AddDataset('tab1:tab2:tab3')
   oFR3:LoadFromFile('test3.frf')
   oFR3:Title := 'Report #3'
   oFR3:ModalPreview := .F.
   oFR3:ShowReport()

   oFR4 := TFreeReport():New()
   oFR4:AddHbDataset('data', 'Eof()', 'DbGoTop()', 'DbSkip()')
   oFR4:LoadFromFile('test4.frf')
   oFR4:Title := 'Report #4'
   oFR4:ModalPreview := .F.
   oFR4:ShowReport()
   
   ? "Loading composite report"
   oFRC := TFreeReport():New(.T.)
   oFRC:AddReport(oFR1)
   oFRC:AddReport(oFR2)
   oFRC:AddReport(oFR3)
   oFRC:AddReport(oFR4)
   oFRC:DoublePass := .T.
   oFRC:Title := 'Test reports 1,2,3 and 4'
   oFRC:ModalPreview := .F.
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