%% @doc
%%   Networking I/O
-module(m_knet).
-compile({parse_transform, category}).

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
   require/2
]).

%%%----------------------------------------------------------------------------   
%%%
%%% monad interface
%%%
%%%----------------------------------------------------------------------------

unit(X) ->
   m_state:unit(X).
   % m_state:unit({ok, X}).

fail(X) ->
   m_state:fail(X).
   % m_state:fail({error, X}).

'>>='(Mx, Fun) ->
   m_state:'>>='(Mx, Fun).
   % join(fmap(Fun, Mx)).

% join(Mx) ->
%    fun(State) ->
%       case Mx(State) of
%          [{ok, Fun} | Y] ->
%             Fun(Y);
%          Error ->
%             Error
%       end
%    end.

% fmap(Fun, Mx) ->
%    fun(State) ->
%       case Mx(State) of
%          [{ok, A} | Y] ->
%             [{ok, Fun(A)}|Y];
%          Error ->
%             Error
%       end
%    end.   

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
      Payload = kscript_codec:encode(
         lens:get(header(<<"Content-Type">>), State0),
         lens:get(payload(), State0)
      ),
      [ _ | State1] = ( m_http:payload(Payload) )(State0),

      %% @todo get timeout from protocol parameters
      [Http | State2] = ( m_http:request() )(State1),
      {Status, Headers, Content} = kscript_codec:decode(Http),
      [Content |
         [identity ||
            lens:put(status(), Status, State2),
            lens:put(headers(), Headers, _),
            lens:put(content(), Content, _)
         ]
      ]
   end.

%%
%% lenses
status() -> lens:c(lens:map(http, #{}), lens:map(status,  none)).
headers() -> lens:c(lens:map(http, #{}), lens:map(headers,    [])).
content() -> lens:c(lens:map(http, #{}), lens:map(content,  none)).

%%
%% extension to manipulate m_http payload
active_socket() ->
   fun(Fun, Map) ->
      Key = maps:get(id, Map),
      lens:fmap(fun(X) -> maps:put(Key, X, Map) end, Fun(maps:get(Key, Map, #{})))
   end.

header(Head) ->
   lens:c(lens:at(spec, #{}), active_socket(), lens:at(head, []), lens:pair(Head, undefined)).

payload() ->
   lens:c(lens:at(spec, #{}), active_socket(), lens:at(payload, undefined)).



%%
%%
require(status, Expect) ->
   require(status(), Expect);

require(Lens, Expect) ->
   fun(State) ->
      case lens:get(Lens, State) of
         Expect ->
            [Expect|State];
         Value  ->
            throw({require, Expect, Value})
      end
   end.




