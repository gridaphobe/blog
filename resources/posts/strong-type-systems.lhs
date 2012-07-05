% On the Benefits of a Strong Type System
% Eric Seidel
% Thu, 21 Jun 2012 00:00:00 -05:00


The other day I encountered a nasty bug (of my own making of course)
at Fluidinfo, where I work part-time. At Fluidinfo we're trying to
make the world writeable by allowing you to add bits of data, which
we call Tags, to anything. So anyway, I wrote some code to remove a
dataset I had imported last year that's taking up a lot of space but
has no users. We use Python at Fluidinfo, so the code looked
something like this

```python
tagIds = # get the list of tags to delete
store.find(TagValue, TagValue.id in tagIds).remove()
```

That code is supposed to take a list of Tags and then remove all of
their instances, simple enough right? Except of course that's not what
it would have done, had I actually run it. The code would have removed
all the tag-instances that shared an ID (in this case an Int as you
might expect) with any of the Tag objects. That's bad, really
bad... Particularly bad because it's entirely possible that the
tag-instances that would have actually been deleted might not have
been instances of the Tags I wanted to delete. What I *wanted* to say
was

```python
tagIds = # get the list of tags to delete
store.find(TagValue, TagValue.tagId in tagIds).remove()
```

Luckily my mistake was caught during code review and it was never
executed! It could have easily slipped through though, only 3
characters differ between the correct line and disaster.

This incident got me thinking, how can we prevent mistakes like that
from even reaching code review? In Python, as in many other
dynamically-typed languages, the standard answer is unit tests. Make
sure every path through the code is tested, and you can be
substantially more confident that your code is correct. I won't argue
that tests aren't important, but I'm not satisfied by that answer in
this case. Why should I have to write a test for that? I can't think
of a reasonable scenario where I would want to delete rows in a table
because their primary key happened to be the same as the primary key
of an item in a completely different table, can you?[^1] I'd rather
have the language come back and tell me that what I'm trying to do
doesn't make the least bit of sense. Luckily some languages can do
exactly that!

[^1]: Some of you may rightly consider this hyperbole because you'd
actually write a test to see if the code deleted the correct
`TagValue`s. That's very true, but it's easy to construct a test-case
that would give you a false positive: start with a fresh DB, create
one Tag and one instance of that Tag. Both will have an ID of 1 and
the test will pass, but not because the code is correct.


Enter Haskell
-------------

Haskell is a pure functional language that I've been playing around
with quite a bit lately. I even wrote my compiler for last semester's
Compilers course in Haskell, which I might write about at some point.
So how does Haskell help us solve my problem? Well, Haskell is
strongly, statically typed, so I just define new types to represent
`TagId`s and `TagValueId`s.

> newtype TagId = TagId Int deriving (Show)
> newtype TagValueId = TagValueId Int deriving (Show)

If you're not familiar with Haskell, don't worry about the `deriving
(Show)` bit, that just tells Haskell I'd like to be able to print
these types out in the REPL. This post is a Literate Haskell file, so
you can actually [download it](/posts/strong-type-systems.lhs), load
it into `ghci` and play around if you feel so inclined. The important
part is

```haskell
newtype TagId = TagId Int
```

which just says that I'm creating a brand new type called `TagId`,
and it's really just a wrapped-up integer. What does this buy me?
Well, now the typechecker knows about two new types and will treat
them as **distinct** from other integer-based types. Here's a simple
function that operates on `TagId`s

> nextTagId (TagId x) = TagId (x + 1)

Nothing too special here, we're just taking one `TagId` and returning
a new one with the wrapped integer incremented, might be useful for
assigning IDs to new Tags. Let's see how it works.

```
ghci> nextTagId (TagId 2)
TagId 3
```

Who'd have thought?! What if I try to give it a plain old integer?

```
ghci> nextTagId 2

<interactive>:6:11:
    No instance for (Num TagId)
      arising from the literal `2' ...
```

Well this error message isn't that helpful, but it does tell us that a
`TagId` cannot be used interchangably with an integer. Good to know,
but not exaclty what we're here for. What if I pass in a `TagValueId`,
which is implemented exactly like `TagId`?

```
ghci> nextTagId (TagValueId 2)

<interactive>:7:12:
    Couldn't match expected type `TagId' with actual type `TagValueId'
    In the return type of a call of `TagValueId'
    In the first argument of `nextTagId', namely `(TagValueId 2)'
    In the expression: nextTagId (TagValueId 2)
```

Awesome! Not only does GHC tell us that we're not allowed to pass in
a `TagValueId`, it also tells us that we probably meant to pass in a
`TagId`. End of post, right? Not exactly...


Why Haskell?
------------

By now you're probably wondering, "What's so special about Haskell? I
can do the same thing in Java or [insert strongly-, statically-typed
language here]." And you certainly can, here's an example of the `TagId`
type in Java.

```java
class TagId {
    TagId(int _i) { i = _i; }
    public int getId() { return i; }
    private int i;
}
```

I could smugly point out that my Haskell code is a whole 4 lines
shorter than the Java code, but let's face it, they both work just
fine. So where's the difference?

You might not want to use the Java code because it boxes up the
integer and allocates space on the heap, when you could have just used
a primitive int. That seems like a trivial concern, but if you
suddenly have to deal with millions of these `TagId`s, it could
quickly become a real concern.

But wait, doesn't the Haskell code do the same thing? No, it does
not. Here's the really cool thing about the `newtype` definition in
Haskell. Since it is restricted to simple "wrappers" like a `TagId`,
Haskell can actually prove that the `TagId` is never accidentally used
as something else, and then throw away everything except the
wrapped-up integer. Let me repeat that, `TagId` and `TagValueId`
**only exist at compile-time**. At run-time they are both just
integers, indistinguishable from one another, or any other "primitive"
integer for that matter. But that's fine because the compiler has
proved that we are using them correctly.

That's pretty damn cool if you ask me.