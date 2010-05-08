---
title: Loops -- Syntax -- Hamlet
---
    1 %ul#employees
    2     $forall employees.company employee
    3         %li $name.employee$

employees.company is parsed as a reference (see [references](references.html)). Each value is bound to the variable employee and then the inner template (line 3) is called.

We allow both simple lists and enumerators to be used as the source value. In order to distinguish between the two, we use an asterisk at the beginning of the reference. For example

    $forall employees.company employee

would require that employees returns a simple list, whereas

    $forall *employees.company employee

would require that it returns an enumerator.
