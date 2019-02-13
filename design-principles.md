
## good cleanness
So experience suggests we should embrace dirtiness. 
Or at least some forms of it; in other ways, the best quick-and-dirty programs are usually quite clean. 
Which kind of dirtiness is bad and which is good? The best kind of quick and dirty programs seem to be ones 
that are mathematically elegant, but missing features-- and particularly features that are inessential 
but deemed necessary for propriety. **Good cleanness is a response to constraints imposed by the problem. 
Bad cleanness is a response to constraints imposed from outside-- by regulations, or the expectations of 
powerful organizations.**

I think these two types of cleanness are not merely separate, but in opposition to one another. 
"The rules," whatever they are, are usually determined by politics; 
you can only obey them at the expense of mathematical elegance. And vice versa. -- Paul G.


## brevity 
what you like about abstraction.
By brevity I don't mean that programs should require fewer characters. 
That counts for something, but it is more important to require **fewer tokens**. -- Paul G.

Mathematical expression(elegant-forms) is always a good way of abstraction for brevity design, 
list and lisp(list processing) are the examples, but remember it is not the goal.

Strategic rules(defined constraints or format, but **redefinable**), is a needed way for brevity design, 
but it only be used for fixing the problems of the limitations of system.

Forced rules MUST never be used, it makes the design less brevity and broken.

**Here, remember:**
> For system framework design, strategic-rules way always play the second role.

**Firstly we should design the prototype framework of the system from elegant-forms way,
we call it "prototype framework"; then we start to design the "perfect framework",
which means we should consider the limitations of prototype framework itself and application fields.
The strategic-rules way used to fix this problem.**


### by-product of vigor
Two sentences with the same meaning:

> There were a great number of dead leaves lying on the ground.

> Dead leaves covered the ground.

In the examples above, that when a sentence is made stronger, it usually becomes shorter.
Thus, brevity is a by-product of vigor(good health).

The excellent products is always within the good health.
So brevity is the key foundation of creating the excellent products.


## clearly understood foundations
In Arc we hope to make programs as short or shorter, but at the same time to build 
the language up in a transparent way from clearly understood foundations. 
We're not doing this (just) out of fastidiousness. You have to build the language 
up transparently if you want users to be able to customize it. 
The chaotic semantics of Perl would make it very hard to add macros.  -- Paul G.


## smoothly lowercase
Ordinary lowercase letters are less work to type than characters like #&%$. 
And lowercase letters are easier to read as well. 
so a language that's easy to type should be a win for them.  -- Paul G.


## integrated with a big application
Every language should be **designed together with a big application** written in it, 
so the designers can see whether the language works. C, for example, 
was sharpened on the systems programming projects that culminated in Unix. 
We're going to use Arc to write a platform for Web-based applications. 
The two will be tightly integrated, like C and Unix.  -- Paul G.

Another example, Monalisp is integrated with db and searching engine.


## easy-to-change
It means the design is easy-to-use, easy-to-extend, easy-for-product-design.
If we want it be easy-to-change, we should create the cleanness framework and extensible brief interface.
**If it's easy-to-change, we get more advantages to win the end-users than other competitors.**


## big-extraordinary-vision-to-the-whole
This is the key for the good design.

**If we want the design to win outside, we need to have a bigger and extraordianry vision from inside.**
To have this vision, we need to zoom in the components, make them brief and abstraction, moreover, to be perfect.
e.g., extracting the common logics as a list-based part.
list is a better expression between logic, abstraction and intuition.
And inside is from without, that is to say, this vision only exists in the world of abstraction and intuition.
Only the great designers have this inborn ability.


## bottom-up-as-extensible-philosophy
> Bottom-up design leads naturally to extensible programs.
If this philosophy is carried all the way up to the topmost layer, that layer becomes 
a programming language for the user. Such a program, where extensibility permeates every level, 
is likely to make a much better programming language than a system which was written 
as a traditional black box, and then made extensible as an afterthought.

> Bottom-up parsing is a strategy for analyzing unknown data relationships that attempts 
to identify the most **fundamental units** first, and then to **infer** higher-order 
structures from them(for example: lisp). Top-down parsers, on the other hand, hypothesize 
general parse tree structures and then consider whether the known fundamental structures 
are compatible with the hypothesis(for example: MS SQL Server, bad). 

> Use bottom-up principle to the foundation of system and system design, 
language is the foundation; but use top-down principle to some special products which is 
complex, emergency and requirement-specified (for example: in the war time, we need a new 
fighting system).

> A individual needs a big-picture vision to the whole for design of the idea; 
Never use top-down way **within planned forced actions by organization** for product design, 
it would make all novel ideas broken, good design and good product impossible.


## create the perfect context

The genius idea always comes from exceptional vision of designer, it shows to realization 
with "The perfect context", this context loops the intuition world, abstraction world and 
realization world together as a new 'world' to provide better solutions for the problems.


## the system is the interface
The philosophy behind this is:

> if the system does not respond to all of the design problem factors, 
no amount of skin-deep "design" will fix the design problems.

Employ a human-centered, whole-systems design methodology that lets user design
problem factors define the function, and lets the function determine the form.


## design the design system
## design the design-framework for design systems
This is the design at the lighest level, also called "strategic design".

Strategic design focuses on roadmaps for innovative products and services.
It requires a high-level vision, insight into future trends, understanding the 
relationship of technology to social change, 
user-centered design expertise, and a "whole systems" design methodology.

The core subsystem(design-framework) of the design system can be integrated into the language.
We use this design-hackable language to design the design system for designing 
for the special products for the end-users.

