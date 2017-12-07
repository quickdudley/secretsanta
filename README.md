# secretsanta

A command line tool for secret santa allocations. Reads a text list of
participants' names and by default outputs one text file per participant. Two
types of annotation are supported.

## Example file:

> John
> Mickey: John
> Elijah? John / Mary
> Mary
> Eve

In this example: Mickey has already jumped the gun and bought a gift for John.
Elijah is blind and either John or Mary will inform him who he is giving to
(if John: this information will be in Mary.txt; if Mary: it will be in John.txt;
otherwise it will be in either John.txt or Mary.txt at random).