---
title: Loops -- Syntax -- Hamlet
---
    1 %ul#employees
    2     $forall company.employees employee
    3         %li $employee.name$

company.employees is parsed as a reference (see [references](references.html)). Each value is bound to the variable employee and then the inner template (line 3) is called.

We allow both simple lists and enumerators to be used as the source value. In order to distinguish between the two, we use an asterisk on the last piece of the references. For example:

    $forall company.employees employee

would require that employees is a simple list, whereas

    $forall company.*employees employee

would require that it be an enumerator.
