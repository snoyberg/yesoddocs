---
title: Loops -- Syntax -- Hamlet
---
    1 %ul#employees
    2     $forall employees.company employee
    3         %li $name.employee$

employees.company is parsed as a reference (see [references](references.html)). Each value is bound to the variable employee and then the inner template (line 3) is called.
