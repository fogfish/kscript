%% @doc
%%   Networking I/O
-module(m_knet).
-compile({parse_transform, category}).

-include_lib("datum/include/datum.hrl").

%% monad interface
-export([unit/1, fail/1, '>>='/2]).

%% BDD interface
-export([
   %% Given
   given/0,
   url/1,
   payload/1, d/1, 

   %% With/When ()
   with/0,
   method/1, x/1,
   header/2, h/2, h/1, 

   %% Then
   then/0,
   require/1,
   require/2
]).

%%
%% request data type
-record(http_request, {
   uri     = ?None    :: _,
   method  = 'GET'    :: _,
   headers = []       :: _,
   content = ?None    :: _,
   so      = []       :: _
}).

%%
%% response data type
-record(http_response, {
   status  = ?None    :: _,
   headers = []       :: _,
   content = ?None    :: _ 
}).

%%%----------------------------------------------------------------------------   
%%%
%%% monad interface
%%%
%%%----------------------------------------------------------------------------

unit(X) ->
   m_state:unit(X).

fail(X) ->
   m_state:fail(X).

'>>='(Mx, Fun) ->
   m_state:'>>='(Mx, Fun).

%%%----------------------------------------------------------------------------   
%%%
%%% state lenses
%%%
%%%----------------------------------------------------------------------------

req_header(Head) ->
   lens:c(lens:at(request), lens:ti(#http_request.headers), lens:pair(Head, ?None)).

req_content() ->
   lens:c(lens:at(request), lens:ti(#http_request.content)).


status() ->
   lens:c(lens:at(response), lens:ti(#http_response.status)).

headers() -> 
   lens:c(lens:at(response), lens:ti(#http_response.headers)).

content() -> 
   lens:c(lens:at(response), lens:ti(#http_response.content)).

%%%----------------------------------------------------------------------------   
%%%
%%% Given
%%%
%%%----------------------------------------------------------------------------
given() -> 
   unit(undefined).

%%
%%
url(Url) ->
   m_http:new(Url).

%%
%%
d(Value) ->
   m_http:d(Value).

payload(Value) ->
   m_http:payload(Value).

%%%----------------------------------------------------------------------------   
%%%
%%% When
%%%
%%%----------------------------------------------------------------------------
with() -> 
   unit(undefined).


%%
%%
x(Mthd) ->
   m_http:x(Mthd).

method(Mthd) ->
   m_http:method(Mthd).

%%
%%
h(Head) ->
   m_http:h(Head).

h(Head, Value) ->
   m_http:h(Head, Value).

header(Head, Value) ->
   m_http:header(Head, Value).


%%%----------------------------------------------------------------------------   
%%%
%%% Then
%%%
%%%----------------------------------------------------------------------------
then() ->
   fun(State0) ->
      %%
      %% encode payload
      Payload = [option ||
         lens:get(req_content(), State0),
         cats:eitherT(htcodec:encode(lens:get(req_header(<<"Content-Type">>), State0), _))
      ],
      [ _ | State1] = ( m_http:payload(Payload) )(State0),

      %% @todo implement defaults
      %%  method - GET
      %%  Accept: */*
      %%  Connection: close

      %% @todo get timeout from protocol parameters
      [Http | State2] = ( m_http:request() )(State1),
      {Status, Headers, _} = hd(Http),
      {ok, Content} = htcodec:decode(Http),
      [Content |
         [identity ||
            cats:unit(State2#{response => #http_response{}}),
            lens:put(status(), Status, _),
            lens:put(headers(), Headers, _),
            lens:put(content(), Content, _)
         ]
      ]
   end.

%%
%% lens based matcher
require(Lens) ->
   fun(State) ->
      case lens:get(lens:c(lens:at(response, #{}), Lens), State) of
         {ok, Expect} ->
            [Expect | State];
         {error, Reason} ->
            throw(Reason)
      end
   end.

%%
%%
require(status, Expect) ->
   require(lens:ti(#http_response.status), Expect);

require(Lens, Expect) ->
   fun(State) ->
      case lens:get(lens:c(lens:at(response, #{}), Lens), State) of
         Expect ->
            [Expect|State];
         Value  ->
            throw({require, Expect, Value})
      end
   end.




