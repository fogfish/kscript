# Networking behaviour

The *do-notation* implements the **Given**/**When**/**Then** and connects cause-and-effect to the networking concept of input/process/output.

**Given** identify the communication context. The **url** is mandatory element that defines a target and communication protocol.  

**When** defines key actions for the interaction with remote host. It specify the protocol parameters, such headers or socket options.

**Then** executes and interactions and observes output of remote hosts, validate its correctness and output the result. The implementation is responsible to transform a binary stream into appropriate format.

## Communication patterns

**request - response**

```erlang
request() ->
   do([kscript ||
      _ /= 'Given'(),
      _ /= url("http://httpbin.org/ip"),
      
      _ /= 'When'(),
      _ /= header("Accept", "application/json"),
      
      _ /= 'Then'(),
      %% return json parsed as map
      %% #{<<"origin">> := <<"192.168.0.1">>} 
   ]).
```

**send - recv**

```erlang
request() ->
   do([kscript ||
      _ /= 'Given'(),
      _ /= url("tcp://httpbin.org:80"),
      
      _ /= 'When'(),
      _ /= send(<<"GET /ip HTTP/1.1\r\nHost: httpbin.org\r\n\r\n">>),
      _ /= recv(1024)
      
      _ /= 'Then'(),
      %% return binary
      %% [
      %%    <<"HTTP/1.1 200 OK\r\n">>,
      %%    <<"Connection: keep-alive\r\n">>,
      %%    ...
      %%    <<"\"origin\": \"192.168.0.1\""}>>
      %% ].
   ]).```



 connect cause-and-effect to connection-oriented L4/L6 protocols (e.g. tcp, ssl)
 connect cause-and-effect to connection-less L4 protocols (e.g. udp)
 connect cause-and-effect to request/response communication patterns (e.g. http, telnet, ...)
 connect cause-and-effect to streaming protocols
