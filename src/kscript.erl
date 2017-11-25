-module(kscript).

-export([
   once/1
]).

once(Script) ->
   try
      [Result | _] = Script(#{}),
      {ok, Result}
   catch throw:Reason ->
      {error, Reason}
   end.
