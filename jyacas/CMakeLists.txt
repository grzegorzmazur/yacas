configure_file (
  "${CMAKE_CURRENT_SOURCE_DIR}/CVersion.java.in"
  "${CMAKE_CURRENT_BINARY_DIR}/CVersion.java"
  )

set (CMAKE_JAVA_INCLUDE_PATH ${CMAKE_CURRENT_BINARY_DIR}/CMakeFiles/jyacas.dir/)

set (JYACAS_SRCS
  ${CMAKE_CURRENT_BINARY_DIR}/CVersion.java
  net/sf/yacas/StdFileInput.java
  net/sf/yacas/StreamGobbler.java
  net/sf/yacas/JarInputFile.java
  net/sf/yacas/CachedStdFileInput.java
  net/sf/yacas/InputStatus.java
  net/sf/yacas/LispInput.java
  net/sf/yacas/LispDefFile.java
  net/sf/yacas/LispDefFiles.java
  net/sf/yacas/InputDirectories.java
  net/sf/yacas/LispHashTable.java
  net/sf/yacas/LispGlobalVariable.java
  net/sf/yacas/LispArgList.java
  net/sf/yacas/GenericClass.java
  net/sf/yacas/LispGenericClass.java
  net/sf/yacas/ArrayClass.java
  net/sf/yacas/AssociationClass.java
  net/sf/yacas/LispInFixOperator.java
  net/sf/yacas/LispPtr.java
  net/sf/yacas/LispObject.java
  net/sf/yacas/LispPrinter.java
  net/sf/yacas/LispError.java
  net/sf/yacas/LispTokenizer.java
  net/sf/yacas/LispParser.java
  net/sf/yacas/LispAtom.java
  net/sf/yacas/LispNumber.java
  net/sf/yacas/LispSubList.java
  net/sf/yacas/LispStandard.java
  net/sf/yacas/LispArityUserFunction.java
  net/sf/yacas/BranchingUserFunction.java
  net/sf/yacas/MacroUserFunction.java
  net/sf/yacas/ListedMacroUserFunction.java
  net/sf/yacas/ListedBranchingUserFunction.java
  net/sf/yacas/LispMultiUserFunction.java
  net/sf/yacas/StringInput.java
  net/sf/yacas/InfixParser.java
  net/sf/yacas/ParsedObject.java
  net/sf/yacas/LispEvaluatorBase.java
  net/sf/yacas/BigNumber.java
  net/sf/yacas/YacasEvaluator.java
  net/sf/yacas/YacasEvalCaller.java
  net/sf/yacas/UserStackInformation.java
  net/sf/yacas/BasicEvaluator.java
  net/sf/yacas/EvalFuncBase.java
  net/sf/yacas/LispOperators.java
  net/sf/yacas/LispUserFunction.java
  net/sf/yacas/LispPtrArray.java
  net/sf/yacas/LispLocalFrame.java
  net/sf/yacas/LispIterator.java
  net/sf/yacas/InfixPrinter.java
  net/sf/yacas/LispEnvironment.java
  net/sf/yacas/MathCommands.java
  net/sf/yacas/YacasPatternPredicateBase.java
  net/sf/yacas/PatternClass.java
  net/sf/yacas/CYacas.java
  net/sf/yacas/YacasConsole.java
  net/sf/yacas/YacasInterpreter.java
  net/sf/yacas/YacasParamMatcherBase.java
  net/sf/yacas/MatchSubList.java
  net/sf/yacas/MatchNumber.java
  net/sf/yacas/MatchAtom.java
  net/sf/yacas/MatchVariable.java
  net/sf/yacas/XmlTokenizer.java
  net/sf/yacas/SubstBehaviourBase.java
  net/sf/yacas/SubstBehaviour.java
  net/sf/yacas/LocalSymbolBehaviour.java
  net/sf/yacas/BackQuoteBehaviour.java
  net/sf/yacas/YacasException.java)

add_jar (jyacas ${JYACAS_SRCS} ENTRY_POINT net/sf/yacas/YacasConsole VERSION ${YACAS_VERSION})

get_target_property(JYACAS_JAR_FILE jyacas JAR_FILE)

add_custom_command(
    TARGET jyacas POST_BUILD
    COMMAND ${Java_JAR_EXECUTABLE} -uf ${JYACAS_JAR_FILE} ${YACAS_SCRIPTS}
    WORKING_DIRECTORY ${PROJECT_SOURCE_DIR})
