---
slug: why-superposition
title: Why did we build Superposition?
description: 
tags: [superposition, why]
---

Superposition started off as a runtime configuration management system.  We wanted to be able to change application configuration at runtime without needing to redeploy applications.

<!-- truncate -->

Once we decided to do that, we immediately became cognizant of the fact that changes are one of the leading cause of software issues and we better build a configuration system that is safe.  And from there were born the core safety principles of Superposition:

1. Type safety
2. Validations
3. Staggered changes

<!--
Most of the products in Juspay are B2B and B2B products typically end up having an innate requirement of having to model the organization or customer hierarchy of its users.  That is to say that as a B2B company - you will end up modelling the organization structure / customers of your customer in some way or the other.  And as a corollary, there emerges a need of having to be able to define and manage configurations in a manner that it cascades through this hierarchy.
-->

The other view of this requirement is that it is fairly common in complex applications that under some domain / business context - the configuration keys take a different value i.e. in other words are overridden.  A common way that many applications solve this is by suffixing/prefixing the key with the context string and defining a new row for the value.  While this approach works - it tends to fall apart when multiple contexts are involved and one has to remember how to construct the key.

At this point - we decided that we had a core kernel of the requirements that our configuration system should solve.

1. Safe 
2. Allow for configurations to vary under different business / domain contexts

We wanted the code that uses configurations to remain dead simple


