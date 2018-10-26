#include "hbfr.ch"
#include "fileio.ch"

FUNCTION Main()

   LOCAL oFreeReport

   ? "Loading library"
   IF !hbfr_LoadLibrary()
      ? "Can not load library."
      RETURN
   ENDIF

   oFreeReport := TFreeReport():New()

   oFreeReport:LoadFromFile( "demo2.frf" )

   dbUseArea( , , "test" )

   // Register harbour dataset - params:
   // cDatasetName - name of dataset
   // cExprCheckEOF - harbor expression which returns a boolean value - .T. if is EOF
   // cExprFirst - harbour expression for jump to the first record
   // cExprNext - harbour expression for jump to the next record
   oFreeReport:AddHbDataset( "test", "test->( Eof() )", "test->( dbGoTop() )", "test->( dbSkip( 1 ) )" )

   oFreeReport:DesignReport()

   oFreeReport:ShowReport()

   test->( dbCloseArea() )

   RETURN NIL


FUNCTION UseFuncs()

   Eof()
   dbGoTop()
   dbSkip()
