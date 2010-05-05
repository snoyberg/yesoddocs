---
title: Maybe -- Syntax -- Hamlet
---
    1 $maybe errorMessage msg
    2     %p.error $msg$

errorMessage is parsed as a [reference](references.html). If the value is Nothing, then no action is taken. If it is Just, then the value is bound to the msg variable and the inner template (line 2) is called.
