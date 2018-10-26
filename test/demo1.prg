#include "hbfr.ch"
#include "fileio.ch"

FUNCTION Main()

   LOCAL oFreeReport

   // harbour variables that will be available in report
   PUBLIC HbNumVar := 123.45                   // [{HbNumVar}]
   PUBLIC HbChrVar := "Hello from Harbour!"    // [{HbChrVar}]

   AltD()

   ? "Loading library"
   IF !hbfr_LoadLibrary()
      ? "Can not load library."
      RETURN
   ENDIF

   oFreeReport := TFreeReport():New()

   oFreeReport:LoadFromFile( "demo1.frf" )

   // Add some values
   oFreeReport:AddValue( "some_number", 543.21 )               // [some_number]
   oFreeReport:AddValue( "some_string", "Some string value" )  // [some_string]

   oFreeReport:DesignReport()

   oFreeReport:ShowReport()

   RETURN NIL
