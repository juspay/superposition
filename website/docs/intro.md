---
sidebar_position: 1
title: Introduction
---

Superposition is a cloud configuration and experimentation management platform that allows software teams to manage their configuration via a central location, run multi-variate experiments for different configuration values and evaluate results of these experiments and conclude them accordingly.

The Superposition platform comes with three components:

* **Context-Aware-Configuration** - a flexible configuration management system that supports contextual overrides for configuration keys
* **Experimentation** - a experimentation management system that allows supplying different configuration values to equal-sized cohorts (facilitating A/B testing)
* **Metrics** - a metrics sub-system that interacts with analytics backends to provide supporting metrics that enable conclusions to be drawn from experiments (TBD)

## Getting started

The fastest way to setup superposition id to download the `docker-compose.yaml` file in the root of this repository and run the following command:

```
docker compose up -d
```

Once you run this command, you'll find Superposition at `localhost:8080`. Play around to understand Superposition better, then dive into the documentation!

## Key highlights

* **Admin UI** - Out of the box administration (and tools) UI for configurations and experiments
* **Rich API support** - every action on the platform to manage configurations / experiments is supported with an accompanying API
* **Safe configuration changes** - support canary testing for releasing configuration changes using experiments
* **Type/Validation support** - Comprehensive type support using json-schema and custom validator function support for configuration values
* **Multi-tenant support** - a single deployment allows multiple tenants to manage their configurations/experiments in a completely isolated manner
* **Authn/Authz support** - control who can make configuration/experimentation changes
