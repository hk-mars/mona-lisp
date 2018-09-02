
## good cleanness
So experience suggests we should embrace dirtiness. 
Or at least some forms of it; in other ways, the best quick-and-dirty programs are usually quite clean. 
Which kind of dirtiness is bad and which is good? The best kind of quick and dirty programs seem to be ones 
that are mathematically elegant, but missing features-- and particularly features that are inessential 
but deemed necessary for propriety. Good cleanness is a response to constraints imposed by the problem. 
Bad cleanness is a response to constraints imposed from outside-- by regulations, or the expectations of 
powerful organizations.

I think these two types of cleanness are not merely separate, but in opposition to one another. 
"The rules," whatever they are, are usually determined by politics; 
you can only obey them at the expense of mathematical elegance. And vice versa. -- Paul G.

## brevity 
what you like about abstraction.
By brevity I don't mean that programs should require fewer characters. 
That counts for something, but it is more important to require fewer tokens. -- Paul G.

Mathematical expression(elegance form) is always a good way of abstraction for design brevity, list and lisp are the examples, but be remember it is not the goal.

Strategical rules(defined constraints or format, but redefinable), is a needed way for design brevity, but it only be used for improving the limitations of system.

Forced rules MUST never be used, it makes the design less brevity and broken.

## clearly understood foundations
In Arc we hope to make programs as short or shorter, but at the same time to build the language up in a transparent way from clearly understood foundations. We're not doing this (just) out of fastidiousness. You have to build the language up transparently if you want users to be able to customize it. The chaotic semantics of Perl would make it very hard to add macros.  -- Paul G.

## smoothly lowercase
Ordinary lowercase letters are less work to type than characters like #&%$. 
And lowercase letters are easier to read as well. 
so a language that's easy to type should be a win for them.  -- Paul G.

## integrated with a big application
Every language should be designed together with a big application written in it, so the designers can see whether the language works. C, for example, was sharpened on the systems programming projects that culminated in Unix. We're going to use Arc to write a platform for Web-based applications. The two will be tightly integrated, like C and Unix.  -- Paul G.

Monalisp would be integrated with db and searching engine.

## easy-to-change
It means the design is easy-to-use, easy-to-entend, easy-for-product-designing.
If we want it be easy-to-change, we should create a cleanness framework and easy-to-use extendable interface for that.
If it was easy-to-change, we get more advantages to win the end-users.


