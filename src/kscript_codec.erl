%% @doc
%%   Content Codec's
-module(kscript_codec).

-export([
   decode/1
]).

decode([{Code, _, Head} = Http | Payload]) ->
   Type    = lens:get(lens:pair(<<"Content-Type">>, undefined), Head),
   Content = decode(binary:split(Type, <<$/>>, []), Payload),
   {Code, Head, Content}.

decode([_, <<"json">>], Payload) ->
   jsx:decode(erlang:iolist_to_binary(Payload), [return_maps]);

decode(_, Payload) ->
   erlang:iolist_to_binary(Payload).