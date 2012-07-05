% 3252 Minutes in Fluidinfo
% Eric Seidel
% Sun, 27 Mar 2011 14:56:33 -05:00

Over the past couple weeks I've been experimenting with a Python script
that crawls [Fluidinfo][fi]. If you're not familiar with Fluidinfo,
read my [previous post][pp] about it, or visit their [website][fi]. Basically,
Fluidinfo is an openly writeable metadata engine, which means that as soon as
you post data to Fluidinfo, others can add to it (or even edit your data if
you wish). Fluidinfo accomplishes this with a simple Objects and Tags
metaphor, in which Objects are anonymous, their meaning coming solely from the
Tags that users add. This is a very social data model (they call it the
"Database with the Heart of a Wiki"), so I wanted to see how users of
Fluidinfo are connected through the objects they tag.

To investigate this, I wrote a little Python script (available at
[GitHub][gh]) to crawl Fluidinfo and find out how users are
connected. Specifically, it counts the number of Objects connecting
any two users. In this case I was not concerned with how many Tags any
user may have placed on an Object, just that they placed at least
one. After working out a few bugs and revising the script to use five
threads instead of just one, I have the results of 3252 Minutes, or
2.25 Days in Fluidinfo.

## Results
I ran the resulting Graphviz dot-file through OmniGraffle using a
Force-Directed layout, and got the following graph (you may want to open it
in a separate tab, it's pretty big).

<a href="/img/fluidinfo_users_graffle.svg"><img class="post"
title="OmniGraffle Result" src="/img/fluidinfo_users_graffle.svg"/></a>

It looks kinda chaotic doesn't it? The first important note is that the
connection lines are color-coded based on how many Objects are connecting the
two users. Black indicates less than 10 objects, blue indicates between 10 and
25, green between 25 and 50, yellow between 50 and 100, and red indicates over
100 connecting objects.

Now let's take a look at the structure of the graph. Right in the
middle of the chaos, we can see two users, _terrycojones_ and _njr_,
with connections to almost every surrounding user. Terrycojones is the
CEO of Fluidinfo and njr blogs extensively on Fluidinfo and how to
classify objects at [AboutTag][at], so it seems reasonable that they
would be focal points of the Fluidinfo web. I'm actually a little
surprised that the graph isn't more centered around njr; he must have
his own bot running around Fluidinfo, because most Objects I look at
have some sort of _njr_ tag ("njr/index" is a popular one if I recall
correctly). Furthermore, there are three fairly distinct clusters of
users around terrycojones and njr. To the left there's a cluster of
domain users (oreilly.com, amazon.com, books.google.com, etc.), which
are all domains that relate to books. This is a good sign because it
indicates that these users are choosing to tag the same objects and
following a consistent About-Tag convention. Below terrycojones is a
loose cluster of technology related users (ycombinator.com,
twitter.com, crunchbase), and above him is a very tight cluster of
ordinary users. I'm really curious to see what Objects are connecting
these users, but that's not something that my crawler picked up on
this run.

Another interesting thing that we can see in this graph is the tagging habits
of users. Most, but not all, users have a line looping back to themselves.
This was actually unintended, but my crawler also picked up how many Objects
each user tagged more than once. Notice that most users don't have more
Objects in common with themselves than they do with other users. This would
seem to indicate that the common behavior is to tag Objects socially, as
opposed to creating a personal database. Of course people could be doing
exactly that with one Tag per Object, in which case my crawler wouldn't have
detected it. One last important note, Fluidinfo has a simple but advanced
Permissions system for Tags, so it's quite possible that users are connected in
ways that are not visible to the public. Of course I wouldn't be able to know
if that's the case here :)

## Thoughts
This was a really interesting experiment for me. A few possible next steps:

1.  Run a new crawler that will also find out _what_ Objects are connecting
    users.
2.  Create a webapp that runs the crawler once a week, and tracks the changes
    over time.
3.  Create a webapp that collects this type of data, but puts it right back in
    Fluidinfo, fulfilling Fluidinfo's Meta-Purpose :p

I'll also briefly list the tools I used to do this.

* My old HTPC: 1.6 GHz Atom processor, 2GB RAM, running Linux Mint 10
* Python 2.6.6
* The Fluid Object Mapper (FOM)
* Graphviz and OmniGraffle

Again the code is available at [GitHub][gh], please let me know if you see any
mistakes. I know it's a bit messy, but I thought it would be rather
unscientific of me to alter the logic after the fact. I also have an SVG
created by Graphviz [here](/images/fluidinfo_users_graphviz.svg). I actually
think it shows the structure a bit better, but I couldn't figure out how to
prevent the lines from going through the users...


[fi]: http://fluidinfo.com
[pp]: /blog/2011/02/fluidcv
[gh]: https://gist.github.com/889274
[at]: http://blog.abouttag.com
