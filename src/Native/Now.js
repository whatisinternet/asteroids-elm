Elm.Native.Now = {};

Elm.Native.Now.make = function(localRuntime) {

  localRuntime.Native = localRuntime.Native || {};


  localRuntime.Native.Now = localRuntime.Native.Now || {};

  if (localRuntime.Native.Now.values) {
    return localRuntime.Native.Now.values;
  }

  var Result = Elm.Result.make(localRuntime);

  return localRuntime.Native.Now.values = {
    loadTime: ((new window.Date).getTime() + Math.floor(Math.random() * (100000000000 - 0)) + 0)
  };

};
