ticktick
========

Ticktick is an id generator for message service. It's now can run as an independent alone service or an embeded one.

## Introduction
Ticktick's id generation rules is like Snowflake of twitter, but with more considerations, 
checkout document DESIGN.md for more details.

## Independent Mode
Ticktick is powered by Mochiweb, which means you can run it by command 
``
start-dev.sh
``
Then retrieve id by command:

``
$ curl -G http://127.0.0.1:8080/id
``

and command to explain an existing id:

``
$ curl -G http://127.0.0.1:8080/explain?id=001ACE3A54400000
``

which you'll get the response:

``
{"version":0,"seconds":1756730,"mseconds":337,"sequence":0,"machine":0,"tag":0}
``
