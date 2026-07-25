---
slug: superposition-genesis
title: Superposition - its genesis?
description: 
tags: [superposition, genesis, introduction]
---

This blog post talks about the genesis of Superposition from a first principles perspective.  What made us build it and the thought process behind structuring it the way we have structured it today.

Superposition started off as a runtime configuration management system.  We wanted to be able to change application configuration at runtime without needing to redeploy applications.

<!-- truncate -->

Once we decided to do that, we immediately became cognizant of the fact that changes are one of the leading cause of software issues and we better build a configuration system that is safe.  And from there were born the core safety principles of Superposition:

1. Type safety
2. Validations
3. Staggered changes

Another requirement that is fairly common in complex applications is that under some domain context - the configuration keys take a different value i.e. in other word the value is overridden under come condition.  This is either managed with control blocks in code (simple if-else statements) or by suffixing/prefixing the key with the context string and defining a new row for the value.  In most cases, when no override exists for a specific context, the typical expectation is to fallback to a default value.

At this point - we decided that we had a core kernel of the requirements that our configuration system should solve.

1. Safety
2. Cascading configuration values

Once we decided to support cascading configuration values, we had to solve for how to define contexts under which configuration values could change and how do we disambiguate between competing contexts that might apply for a single domain flow.

And so we decided to model how to define contexts in a concrete manner.  Contexts are simple boolean expressions involving key attributes that matter to your domain.  We chose to call these key attributes `dimensions`.  Dimensions are also defined with their associated types to ensure that values set for dimensions in contexts are validated and type-safe.  Once you have dimensions, it becomes inevitable that we will need to disambiguate between dimensions as more than one dimension can evaluate to true in a particular domain workflow i.e. for e.g. the `client platform` dimension could be `ios` and `user` dimension could be `alice` at the same time.  If one has different values defined for each of these contexts separately - the system needs to clearly identify which one wins.  Towards this we decided to force linear prioritization of the dimensions.  While this choice might seem constraining - we believe at a deeper level most systems end-up having a priority between these dimensions implicitly in code if it is not explicitly captured.

So we refined the definition of what we wanted to build.  We wanted to build a configuration system that would enable the following:

1. Safely change configuration values
2. Unambiguous cascading of configuration values

As we did this - we noticed an interesting nexus between cascading configurations and the need to be able to stagger configuration changes.  Staggering of configuration changes can be treated as an experimental override if we make the experimental variation as part of the context.  Hence, the cascading nature of the configuration was not just an independent requirement, it also formed the basis for supporting configuration staggering.
