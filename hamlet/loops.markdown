---
title: Loops -- Syntax -- Hamlet
---
    1 %ul#employees
    2     $forall company.employees employee
    3         %li $employee.name$

company.employees is parsed as a reference (see Content Parsing above). Each value is bound to the variable employee and then the inner template (line 3) is called.

For efficiency, we use enumerators for the type of employees. See the synopsis section for a full example.
