# K.script

`K.script` is a pure functional domain specific language to script networking I/O in style of Behaviour Driven Development (BDD).

## Inspiration

Microservices have become a design style to evolve systems architecture in parallel, implement stable and consistent interfaces. This architecture style brings additional complexity and new problems. The system communication over network have a higher cost in terms of network latency and message processing. We need an ability to quantitatively evaluate and trade-off the architecture to ensure competitive end-to-end latency of software solutions. The `K.script` is developed to perform a **quality assessment** of distributed software architecture using simple description of *networking behaviour*.

For example, we can express `curl` command in BDD notation

```bash
export GIVEN='http://example.com'
export WHEN="-H 'Accept-Language: en'"
export THEN='-o/tmp/example.html'

curl ${GIVEN} ${WHEN} ${THEN} 
``` 


A scripting language is the challenge to solve here. An expressive language is required to cover the variety of communication protocols and behaviour use-cases. A pure functional languages fits very well to express communication behaviour. It gives a rich techniques to hide the communication complexity using *monads* as abstraction. The IO-monads helps us to compose a chain of network operations and represent them as pure computation.

 
### scripting language

The *networking behaviour* is defined either using Erlang flavoured syntax (a valid Erlang code) or YAML scripting. This design has been driven by

* the ability to spawn a huge number networking sessions, real-time data processing and accuracy of measurements are major requirements that impacted on selection of Erlang as a primary runtime environment.

* the adoption of scripting requires that whole team understand what is wanted (BDD implies that natural language is used to specify scenarious). Therefore, `K.script` supports the definition of the behaviour in non-technical language (ubiquitous language) such as YAML, which is eventually compilable to native code. The usage of YAML for scripting has been proven by various Infrastructure-as-a-Code solutions.  


You script networking using **Behaviour as a Code** paradigm.


`K.script` provides parsing, compilation, and the debugging of *networking behaviour*. The advanced development requires a basic understanding of functional programming concepts and knowledge of Erlang syntax:

* [Erlang language tutorial](http://learnyousomeerlang.com/starting-out-for-real)
* [Erlang modules tutorial](http://learnyousomeerlang.com/modules#what-are-modules)
* [Erlang expressions](http://erlang.org/doc/reference_manual/expressions.html)

However, **YAML** is an advised syntax for *behaviour driven development* that builds a shared understanding about the system. 

### do-notation

`K.script` uses the "do"-notation, so called monadic binding form. It is well know in functional programming languages such as [Haskell](https://en.wikibooks.org/wiki/Haskell/do_notation), [Scala](http://docs.scala-lang.org/tutorials/tour/sequence-comprehensions.html) and [Erlang](https://github.com/fogfish/datum/blob/master/doc/monad.md). The *networking behaviour* is a collection of composed `do-notation` in context of a [state monad](https://acm.wustl.edu/functional/state-monad.php).

The *do-notation* implements the **Given**/**When**/**Then** and connects cause-and-effect to the networking concept of input/process/output:

1. **Given** identify the communication context and known state for the use-case
2. **When** defines key actions for the interaction with remote host.
3. **Then** observes output of remote hosts, validate its correctness and output the result.

The *do-notation* is a sequence of actions that passes results of computation downstream in the binding sequence. This abstraction allows to isolate the definition of *behaviour* from its implementation.

Let's look on the following example

```bash
# request langing page
curl http://example.com -H 'Accept-Language: en'
```

The *do-notation* of networking I/O using YAML

```yaml
Scenario: |
  request landing page

Given:
  url: http://example.com

When:
  header:
    Accept-Language: en
```

and equivalent I/O declaration in Erlang flavoured syntax

```erlang
request_lannding_page() ->
   do([kscript ||
      _ /= 'Given'(),
      _ /= url("http://example.com"),
      
      _ /= 'When'(),
      _ /= header('Accept-Language', en),
      
      _ /= 'Then'(),
      return(_)
   ]).
```


## Key features

tbd.

## Getting started

tbd.

## Next steps

tbd.

## How to contribute

tbd.

## Changelog

tbd.

## License

Copyright 2015 Dmitry Kolesnikov

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of the License at http://www.apache.org/licenses/LICENSE-2.0.

Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language governing permissions and limitations under the License.
