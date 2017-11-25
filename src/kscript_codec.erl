%% @doc
%%   Content Codec's
-module(kscript_codec).
-compile({parse_transform, category}).

-export([
   decode/1,
   encode/2
]).

%%
%%
decode([{Code, _, Head} = Http | Payload]) ->
   Type    = lens:get(lens:pair(<<"Content-Type">>, undefined), Head),
   Content = decode(Type, Payload),
   {Code, Head, Content}.

decode(<<"application/json">>, Payload) ->
   jsx:decode(erlang:iolist_to_binary(Payload), [return_maps]);

decode(<<"application/x-www-form-urlencoded">>, Payload) ->
   {ok, Form} = decode_form(Payload),
   Form;

decode(_, Payload) ->
   erlang:iolist_to_binary(Payload).


%%
%%
encode(<<"application/json">>, Payload) ->
   jsx:encode(Payload);

encode(<<"application/x-www-form-urlencoded">>, Payload) ->
   {ok, Form} = encode_form(Payload),
   Form;

encode(_, Payload) ->
   Payload.





encode_form(Form) ->
   try
      {ok, [identity ||
         maps:to_list(Form),
         lists:map(fun to_pair/1, _),
         scalar:s(lists:join(<<$&>>, _))
      ]}
   catch _:_ ->
      {error, {badarg, payload}}
   end.

to_pair(Pair) ->
   scalar:s(
      lists:join(<<$=>>, 
         [uri:escape(X) || X <- erlang:tuple_to_list(Pair)]
      )
   ).


decode_form(Form) ->
   try
      {ok, [identity ||
         binary:split(scalar:s(Form), <<$&>>, [trim, global]),
         lists:map(fun as_pair/1, _),
         maps:from_list(_)
      ]}
   catch _:_ ->
      {error, {badarg, payload}}
   end.
   

as_pair(Pair) ->
   erlang:list_to_tuple(
      [uri:unescape(X) || X <- binary:split(Pair, <<$=>>)]
   ).
