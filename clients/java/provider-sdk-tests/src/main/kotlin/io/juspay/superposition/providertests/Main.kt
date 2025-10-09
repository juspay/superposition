package io.juspay.superposition.providertests

import dev.openfeature.sdk.OpenFeatureAPI
import dev.openfeature.sdk.ImmutableContext
import dev.openfeature.sdk.Value
import io.juspay.superposition.client.SuperpositionClient
import io.juspay.superposition.model.*
import io.juspay.superposition.openfeature.SuperpositionOpenFeatureProvider
import io.juspay.superposition.openfeature.SuperpositionProviderOptions
import io.juspay.superposition.openfeature.options.RefreshStrategy
import kotlinx.coroutines.runBlocking
import software.amazon.smithy.java.client.core.endpoint.EndpointResolver
import software.amazon.smithy.java.core.schema.Unit
import software.amazon.smithy.java.core.serde.document.Document
import software.amazon.smithy.java.auth.api.identity.TokenIdentity
import software.amazon.smithy.java.client.core.auth.identity.IdentityResolver
import software.amazon.smithy.java.client.core.auth.identity.IdentityResult
import software.amazon.smithy.java.auth.api.AuthProperties
import java.util.concurrent.CompletableFuture
import kotlin.system.exitProcess

const val ORG_ID = "localorg"
const val WORKSPACE_ID = "kotlinprovidertest"

data class SuperpositionSDKConfig(
    val endpoint: String = "http://localhost:8080",
    val token: String = "12131"
)

class Main {
    private val config = SuperpositionSDKConfig()
    private lateinit var client: SuperpositionClient

    init {
        setupClient()
    }

    private fun setupClient() {
        val identityResolver = object : IdentityResolver<TokenIdentity> {
            override fun resolveIdentity(requestProperties: AuthProperties): CompletableFuture<IdentityResult<TokenIdentity>> {
                return CompletableFuture.completedFuture(
                    IdentityResult.of(TokenIdentity.create(config.token))
                )
            }

            override fun identityType(): Class<TokenIdentity> = TokenIdentity::class.java
        }

        client = SuperpositionClient.builder()
            .endpointResolver(EndpointResolver.staticEndpoint(config.endpoint))
            .addIdentityResolver(identityResolver)
            .build()
    }

    private fun createWorkspace(orgId: String, workspaceName: String) {
        val input = CreateWorkspaceInput.builder()
            .orgId(orgId)
            .workspaceName(workspaceName)
            .workspaceAdminEmail("test@tests.com")
            .workspaceStatus(WorkspaceStatus.ENABLED)
            .strictMode(true)
            .allowExperimentSelfApproval(true)
            .autoPopulateControl(true)
            .build()

        try {
            val future = CompletableFuture.supplyAsync {
                client.createWorkspace(input)
            }
            future.thenAccept { response ->
                println("Workspace created!\n$response")
            }
            future.join()
        } catch (e: Exception) {
            println("An exception occurred while creating a workspace: $e")
            throw e
        }
    }

    private fun createDimensions(orgId: String, workspaceId: String) {
        val dimensions = listOf(
            CreateDimensionInput.builder()
                .workspaceId(workspaceId)
                .orgId(orgId)
                .dimension("name")
                .position(1)
                .schemaMember(mapOf("type" to Document.of("string")))
                .description("customer name dimension")
                .changeReason("adding name dimension")
                .dimensionType(DimensionType.builder().regulaR(Unit.getInstance()).build())
                .build(),

            CreateDimensionInput.builder()
                .workspaceId(workspaceId)
                .orgId(orgId)
                .dimension("city")
                .position(2)
                .schemaMember(mapOf("type" to Document.of("string")))
                .description("city dimension")
                .changeReason("adding city dimension")
                .dimensionType(DimensionType.builder().regulaR(Unit.getInstance()).build())
                .build(),

            CreateDimensionInput.builder()
                .workspaceId(workspaceId)
                .orgId(orgId)
                .dimension("customers")
                .position(3)
                .schemaMember(mapOf(
                    "type" to Document.of("string"),
                    "enum" to Document.of(listOf(
                        Document.of("platinum"),
                        Document.of("gold"),
                        Document.of("otherwise")
                    )),
                    "definitions" to Document.of(mapOf(
                        "platinum" to Document.of(mapOf(
                            "in" to Document.of(listOf(
                                Document.of(mapOf("var" to Document.of("name"))),
                                Document.of(listOf(Document.of("Agush"), Document.of("Sauyav")))
                            ))
                        )),
                        "gold" to Document.of(mapOf(
                            "in" to Document.of(listOf(
                                Document.of(mapOf("var" to Document.of("name"))),
                                Document.of(listOf(Document.of("Angit"), Document.of("Bhrey")))
                            ))
                        ))
                    ))
                ))
                .description("customers dimension")
                .changeReason("adding customers dimension")
                .dimensionType(DimensionType.builder().localCohort("name").build())
                .build()
        )

        println("Creating dimensions:")
        for (dimension in dimensions) {
            try {
                val future = CompletableFuture.supplyAsync {
                    client.createDimension(dimension)
                }
                future.thenAccept { response ->
                        println("  - Created dimension: ${response.dimension()}")
                }
                future.join()
            } catch (e: Exception) {
                println("Error occurred while creating dimension: ${dimension.dimension()}, $e")
                throw e
            }
        }
    }

    private fun createDefaultConfigs(orgId: String, workspaceId: String) {
        val configs = listOf(
            CreateDefaultConfigInput.builder()
                .key("price")
                .value(Document.of(10000))
                .schemaMember(mapOf(
                    "type" to Document.of("number"),
                    "minimum" to Document.of(0)
                ))
                .description("price as a positive number")
                .changeReason("adding price config")
                .workspaceId(workspaceId)
                .orgId(orgId)
                .build(),

            CreateDefaultConfigInput.builder()
                .key("currency")
                .value(Document.of("Rupee"))
                .schemaMember(mapOf(
                    "type" to Document.of("string"),
                    "enum" to Document.of(listOf(
                        Document.of("Rupee"),
                        Document.of("Dollar"),
                        Document.of("Euro")
                    ))
                ))
                .description("currency as an enum")
                .changeReason("adding currency config")
                .workspaceId(workspaceId)
                .orgId(orgId)
                .build()
        )

        println("Creating default configs:")
        for (config in configs) {
            try {
                val future = CompletableFuture.supplyAsync {
                    client.createDefaultConfig(config)
                }
                future.thenAccept { response ->
                    println("  - Created config: ${response.key()}")
                }
                future.join()
            } catch (e: Exception) {
                println("Error occurred while creating config: ${config.key()}, $e")
                throw e
            }
        }
    }

    private fun createOverrides(orgId: String, workspaceId: String) {
        val overrides = listOf(
            CreateContextInput.builder()
                .workspaceId(workspaceId)
                .orgId(orgId)
                .context(mapOf("city" to Document.of("Boston")))
                .override(mapOf("currency" to Document.of("Dollar")))
                .description("Bostonian")
                .changeReason("testing")
                .build(),

            CreateContextInput.builder()
                .workspaceId(workspaceId)
                .orgId(orgId)
                .context(mapOf("city" to Document.of("Berlin")))
                .override(mapOf("currency" to Document.of("Euro")))
                .description("Berlin")
                .changeReason("testing")
                .build(),

            CreateContextInput.builder()
                .workspaceId(workspaceId)
                .orgId(orgId)
                .context(mapOf("customers" to Document.of("platinum")))
                .override(mapOf("price" to Document.of(5000)))
                .description("platinum customer")
                .changeReason("testing")
                .build(),

            CreateContextInput.builder()
                .workspaceId(workspaceId)
                .orgId(orgId)
                .context(mapOf("customers" to Document.of("gold")))
                .override(mapOf("price" to Document.of(8000)))
                .description("gold customers")
                .changeReason("testing")
                .build(),

            CreateContextInput.builder()
                .workspaceId(workspaceId)
                .orgId(orgId)
                .context(mapOf(
                    "name" to Document.of("karbik"),
                    "customers" to Document.of("otherwise")
                ))
                .override(mapOf("price" to Document.of(1)))
                .description("edge case customer karbik")
                .changeReason("testing")
                .build()
        )

        println("Creating overrides:")
        for (override in overrides) {
            try {
                val future = CompletableFuture.supplyAsync {
                    client.createContext(override)
                }
                future.thenAccept { response ->
                    println("Created override: ${response.value()}")
                }
                future.join()
            } catch (e: Exception) {
                println("Error occurred while creating override: $override, $e")
                throw e
            }
        }
    }

    private fun createExperiments(orgId: String, workspaceId: String) {
        val experiments = listOf(
            CreateExperimentInput.builder()
                .workspaceId(workspaceId)
                .orgId(orgId)
                .name("testexperiment")
                .context(mapOf("city" to Document.of("Bangalore")))
                .variants(listOf(
                    Variant.builder()
                        .id("testexperiment-control")
                        .variantType(VariantType.CONTROL)
                        .overrides(Document.of(mapOf("price" to Document.of(10000))))
                        .build(),
                    Variant.builder()
                        .id("testexperiment-experimental")
                        .variantType(VariantType.EXPERIMENTAL)
                        .overrides(Document.of(mapOf("price" to Document.of(8800))))
                        .build()
                ))
                .description("test experimentation")
                .changeReason("a reason")
                .build()
        )

        for (experiment in experiments) {
            try {

                val future = CompletableFuture.supplyAsync {
                    client.createExperiment(experiment)
                }
                future.thenAccept { response ->
                    println("Created experiment: $response")
                    val rampInput = RampExperimentInput.builder()
                        .workspaceId(workspaceId)
                        .orgId(orgId)
                        .id(response.id())
                        .changeReason("ramp the experiment")
                        .trafficPercentage(25)
                        .build()
    
                    val rampFuture = CompletableFuture.supplyAsync {
                        client.rampExperiment(rampInput)
                    }.thenAccept { rampResponse ->
                        println("Experiment ramped: $rampResponse")
                    }
                    rampFuture.join()
                }
                future.join()
            } catch (e: Exception) {
                println("Error occurred while creating experiment: $experiment, $e")
                throw e
            }
        }
    }

    private fun setupWithSDK(orgId: String, workspaceId: String) {
        println("\n=== Setting up test environment ===\n")
        createWorkspace(orgId, workspaceId)
        createDimensions(orgId, workspaceId)
        createDefaultConfigs(orgId, workspaceId)
        createOverrides(orgId, workspaceId)
        createExperiments(orgId, workspaceId)
        println("\n=== Setup complete ===\n")
    }

    private fun runDemo(orgId: String, workspaceId: String) {
        val providerOptions = SuperpositionProviderOptions.builder()
            .orgId(orgId)
            .workspaceId(workspaceId)
            .endpoint("http://localhost:8080")
            .token("12345678")
            .refreshStrategy(RefreshStrategy.Polling.of(10000, 5000))
            .build()

        try {
            println("\n=== Starting OpenFeature tests ===\n")
            println("Running on JVM: ${System.getProperty("java.version")}")

            val provider = SuperpositionOpenFeatureProvider(providerOptions)
            println("Provider created successfully")

            // Initialize the provider
            OpenFeatureAPI.getInstance().setProviderAndWait(provider)
            println("Provider initialized successfully\n")

            val ofClient = OpenFeatureAPI.getInstance().client

            // Test 1: Default values (no context)
            println("Test 1: Default values (no context)")
            run {
                val context = ImmutableContext()
                val price = ofClient.getDoubleValue("price", 0.0, context)
                val currency = ofClient.getStringValue("currency", "", context)

                check(price == 10000.0) { "Default price should be 10000, got $price" }
                check(currency == "Rupee") { "Default currency should be Rupee, got $currency" }
                println("  ✓ Test passed\n")
            }

            // Test 2: Platinum customer - Agush, no city
            println("Test 2: Platinum customer - Agush (no city)")
            run {
                val context = ImmutableContext(mapOf("name" to Value("Agush")))
                val price = ofClient.getDoubleValue("price", 0.0, context)
                val currency = ofClient.getStringValue("currency", "", context)

                check(price == 5000.0) { "Price should be 5000 (platinum customer), got $price" }
                check(currency == "Rupee") { "Currency should be default Rupee, got $currency" }
                println("  ✓ Test passed\n")
            }

            // Test 3: Platinum customer - Sauyav, with city Boston
            println("Test 3: Platinum customer - Sauyav with city Boston")
            run {
                val context = ImmutableContext(mapOf(
                    "name" to Value("Sauyav"),
                    "city" to Value("Boston")
                ))
                val price = ofClient.getDoubleValue("price", 0.0, context)
                val currency = ofClient.getStringValue("currency", "", context)

                check(price == 5000.0) { "Price should be 5000, got $price" }
                check(currency == "Dollar") { "Currency should be Dollar, got $currency" }
                println("  ✓ Test passed\n")
            }

            // Test 4: Regular customer - John (no city)
            println("Test 4: Regular customer - John (no city)")
            run {
                val context = ImmutableContext(mapOf("name" to Value("John")))
                val price = ofClient.getDoubleValue("price", 0.0, context)
                val currency = ofClient.getStringValue("currency", "", context)

                check(price == 10000.0) { "Price should be default 10000, got $price" }
                check(currency == "Rupee") { "Currency should be default Rupee, got $currency" }
                println("  ✓ Test passed\n")
            }

            // Test 5: Platinum customer - Sauyav with city Berlin
            println("Test 5: Platinum customer - Sauyav with city Berlin")
            run {
                val context = ImmutableContext(mapOf(
                    "name" to Value("Sauyav"),
                    "city" to Value("Berlin")
                ))
                val price = ofClient.getDoubleValue("price", 0.0, context)
                val currency = ofClient.getStringValue("currency", "", context)

                check(price == 5000.0) { "Price should be 5000, got $price" }
                check(currency == "Euro") { "Currency should be Euro in Berlin, got $currency" }
                println("  ✓ Test passed\n")
            }

            // Test 6: Regular customer - John with city Boston
            println("Test 6: Regular customer - John with city Boston")
            run {
                val context = ImmutableContext(mapOf(
                    "name" to Value("John"),
                    "city" to Value("Boston")
                ))
                val price = ofClient.getDoubleValue("price", 0.0, context)
                val currency = ofClient.getStringValue("currency", "", context)

                check(price == 10000.0) { "Price should be default 10000, got $price" }
                check(currency == "Dollar") { "Currency should be Dollar in Boston, got $currency" }
                println("  ✓ Test passed\n")
            }

            // Test 7: Edge case customer - karbik (specific override)
            println("Test 7: Edge case customer - karbik (specific override)")
            run {
                val context = ImmutableContext(mapOf("name" to Value("karbik")))
                val price = ofClient.getDoubleValue("price", 0.0, context)
                val currency = ofClient.getStringValue("currency", "", context)

                check(price == 1.0) { "Price should be 1 for karbik, got $price" }
                check(currency == "Rupee") { "Currency should be default Rupee, got $currency" }
                println("  ✓ Test passed\n")
            }

            // Test 8: Edge case customer - karbik with city Boston
            println("Test 8: Edge case customer - karbik with city Boston")
            run {
                val context = ImmutableContext(mapOf(
                    "name" to Value("karbik"),
                    "city" to Value("Boston")
                ))
                val price = ofClient.getDoubleValue("price", 0.0, context)
                val currency = ofClient.getStringValue("currency", "", context)

                check(price == 1.0) { "Price should be 1 for karbik, got $price" }
                check(currency == "Dollar") { "Currency should be Dollar in Boston, got $currency" }
                println("  ✓ Test passed\n")
            }

            println("\n=== All tests passed! ===\n")
            exitProcess(0)
        } catch (error: Exception) {
            println("\n❌ Error running tests: $error")
            error.printStackTrace()
            throw error
        } finally {
            OpenFeatureAPI.getInstance().shutdown()
            println("OpenFeature closed successfully")
        }
    }

    fun run() {
        println("Starting Superposition OpenFeature demo and tests (Kotlin)...")

        try {
            setupWithSDK(ORG_ID, WORKSPACE_ID)
            runDemo(ORG_ID, WORKSPACE_ID)
        } catch (error: Exception) {
            println("\n❌ Test suite failed: $error")
            error.printStackTrace()
            exitProcess(1)
        }
    }
}

fun main() {
    Main().run()
}
