
#include <stdio.h> //TODO remove, debug!

#include "lisptype.h"
#include "lispenvironment.h"
#include "lispatom.h"
#include "standard.h"
#include "arggetter.h"
#include "lispplugin.h"
#include "platmath.h"
#include "stubs.h"
#include "genericstructs.h"
#include "infixparser.h"
#include "lispeval.h"
#include "errors.h"

#include "filescanner.h"

#define InternalEval aEnvironment.iEvaluator->Eval
#define RESULT aEnvironment.iStack.GetElement(aStackTop)
#define ARGUMENT(i) aEnvironment.iStack.GetElement(aStackTop+i)

static void ScanFiles(LispEnvironment& aEnvironment, LispInt aStackTop)
{
  CHK_CORE(aEnvironment.iSecure == 0, KLispErrSecurityBreach);

  // Get the input, consisting of three strings
  CHK_ISSTRING_CORE(ARGUMENT(1),1);
  LispString baseDir;
  InternalUnstringify(baseDir, ARGUMENT(1).Get()->String());

  CHK_ISSTRING_CORE(ARGUMENT(2),2);
  LispString subDir;
  InternalUnstringify(subDir, ARGUMENT(2).Get()->String());

  CHK_ISSTRING_CORE(ARGUMENT(3),3);
  LispString callFunc;
  InternalUnstringify(callFunc, ARGUMENT(3).Get()->String());

//printf("Basedir = %s\nSubdir = %s\nCall = %s\n",baseDir.String(),subDir.String(),callFunc.String());

  CFileScanner scanner;
  CFileNode* node = scanner.First(baseDir.String(),subDir.String());
  while (node != NULL)
  {
    //TODO @@@ potential buffer overflow!
    char buf[1024];
    sprintf(buf,"%s(\"%s\",\"%s\",%s);\n",callFunc.String(),baseDir.String(),node->FullName(),node->IsDirectory()? "True":"False");
    LispPtr expr;
    PARSE(expr,buf);
    InternalEval(aEnvironment, RESULT, expr);
    node = scanner.Next();
  }

  InternalTrue(aEnvironment, RESULT);
}



class FileScannerPlugin : public LispPluginBase
{
public:
    virtual void Add(LispEnvironment& aEnvironment);
    virtual void Remove(LispEnvironment& aEnvironment);
};
void FileScannerPlugin::Add(LispEnvironment& aEnvironment)
{
  aEnvironment.SetCommand(ScanFiles, "ScanFiles",3,YacasEvaluator::Function | YacasEvaluator::Fixed);
}

void FileScannerPlugin::Remove(LispEnvironment& aEnvironment)
{
  aEnvironment.RemoveCoreCommand("ScanFiles");
}


extern "C" {

LispPluginBase* make_filescanner(void)
{
    return NEW FileScannerPlugin;
}

}





