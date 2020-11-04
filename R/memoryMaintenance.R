#These functions are used in JASPengine to clean up memory after running an analysis.

.doNotRemoveFromGlobalEnv <- list2env(list(initialVars=c(), modules=c()))

.initializeDoNotRemoveList <- function() {
  #This function is called after initializing the engine, R and some jasp related stuff and we fill .doNotRemoveFromGlobalEnv with globals we want to keep

  .doNotRemoveFromGlobalEnv$initialVars <- ls(all.names = TRUE, envir = .GlobalEnv);

  print("Initializing do not remove list and it is now:")
  print(.doNotRemoveFromGlobalEnv$initialVars)
}

.addModuleToDoNotRemove <- function(module) {

  .doNotRemoveFromGlobalEnv$modules <- append(.doNotRemoveFromGlobalEnv$modules, module);
}

.cleanEngineMemory <- function()
{
  print('purging global environment R');			

  #print("Do not remove list and it is now:")
  doNotRemoveAnyOfThese <- c(.doNotRemoveFromGlobalEnv$initialVars, .doNotRemoveFromGlobalEnv$modules)
  #print(doNotRemoveAnyOfThese)

  currentList <- ls(all.names = TRUE, envir = .GlobalEnv);
  currentList <- currentList[!(currentList %in% doNotRemoveAnyOfThese)];
  print('Variables about to be removed from global env:');
  print(currentList);

  rm(list = currentList, pos = .GlobalEnv);
  gc();
}
