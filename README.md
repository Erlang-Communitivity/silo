Silo - A simple Atom entry data store
---

This is the beginning of an erlang app that can be added to any 
erlang project to enable easy storage and retrieval of Atom
entries without worrying about how those entries are stored.

Plans include a redis and couchdb backends in separate projects.
The default backend will remain dets to provide a lightweight
implementation usable for experimentation or prototyping.

The next things to do are:
.. build support with rebar
.. create app supporting files to wrap gen server
.. add unit tests
.. expand readme.md with build and test directions
.. add JSON format support
.. add fetch_all that returns an Atom document containing all entries
.. add fetch_matching that takes an xpath string and returns all entries that match it
.. add fetch_filtered that takes an xpath string and returns all entries that don't match it
