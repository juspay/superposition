# <a href="https://juspay.io/open-source/superposition" target="_blank"> Superposition </a> 

<p align="center">
<img src="https://juspay.io/images/superposition/logo.jpg" alttext="Superposition Logo" width="400">
</p>

[![GitHub commit activity](https://img.shields.io/github/commit-activity/m/juspay/superposition)](https://github.com/juspay/superposition/pulse) 
[![GitHub last commit](https://img.shields.io/github/last-commit/juspay/superposition)](https://github.com/juspay/superposition/commits/main)  ![X (formerly Twitter) Follow](https://img.shields.io/twitter/follow/superpositionJP)  [![Discord badge](https://img.shields.io/discord/1280216553350107258?label=Discord&logo=Discord)](https://discord.gg/jNeUJR9Bwr) 


Superposition is a cloud configuration and experimentation management platform that allows software teams to manage their configuration via a central location, run multi-variate experiments for different configuration values and evaluate results of these experiments and conclude them accordingly.

The Superposition platform comes with three components:

* **Context-Aware-Configuration** - a flexible configuration management system that supports contextual overrides for configuration keys
* **Experimentation** - a experimentation management system that allows supplying different configuration values to equal-sized cohorts (facilitating A/B testing)
* **Metrics** - a metrics sub-system that interacts with analytics backends to provide supporting metrics that enable conclusions to be drawn from experiments (TBD)

## Getting started

The fastest way to setup superposition along with a demo-app is to use the following docker command:

```
docker run -d -p 8081:9090 -p 8080:8080 datron1/superposition-demo-app:latest
```

Once you run this command, you'll find the demo app at `localhost:8081` and Superposition at `localhost:8080`. Play around to understand Superposition better, then dive into the documentation below!

## Detailed documentation
1. [Context-Aware-Configuration](docs/context-aware-config.md)
2. [Experimentation](docs/experimentation.md)
3. [Metrics](docs/metrics.md)
4. [Client Context-Aware-Configuration](docs/client-context-aware-configuration.md)
4. [Client Experimentation](docs/client-experimentation.md)
5. [Local setup](docs/setup.md)
6. [Context-Aware-Configuration API Ref - Postman Collection](postman/cac.postman_collection.json)
7. [Experimentation API Ref - Postman Collection](postman/experimentation-platform.postman_collection.json)
8. [TOML formatted Context-Aware-Configuration example](docs/cac-toml.md)
9. [Hitchiker's Guide to Create a New Client](docs/creating-client.md)

## Key highlights
* **Admin UI** - Out of the box administration (and tools) UI for configurations and experiments
* **Rich API support** - every action on the platform to manage configurations / experiments is supported with an accompanying API
* **Safe configuration changes** - support canary testing for releasing configuration changes using experiments
* **Type/Validation support** - Comprehensive type support using json-schema and custom validator function support for configuration values
* **Multi-tenant support** - a single deployment allows multiple tenants to manage their configurations/experiments in a completely isolated manner
* **Authn/Authz support** - control who can make configuration/experimentation changes

## Email us
* [superposition@juspay.in](mailto:superposition@juspay.in)
