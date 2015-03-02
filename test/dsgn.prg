#include "hbfr.ch"

function main(cParam1, cParam2, cParam3, cParam4)
   LOCAL oFR
   oFR := TFreeReport():New()
   IF !Empty(cParam1)
      oFR:AddDataset(cParam1)
   ENDIF
   IF !Empty(cParam2)
      oFR:AddDataset(cParam2)
   ENDIF
   IF !Empty(cParam3)
      oFR:AddDataset(cParam3)
   ENDIF
   IF !Empty(cParam4)
      oFR:AddDataset(cParam4)
   ENDIF
   oFR:DesignReport()
   oFR := NIL