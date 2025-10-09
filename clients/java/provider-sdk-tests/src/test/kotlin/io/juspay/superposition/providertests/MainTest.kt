package io.juspay.superposition.providertests

import org.junit.jupiter.api.Test
import org.junit.jupiter.api.TestInstance
import org.junit.jupiter.api.Timeout
import java.util.concurrent.TimeUnit

@TestInstance(TestInstance.Lifecycle.PER_CLASS)
class MainTest {

    @Test
    @Timeout(value = 5, unit = TimeUnit.MINUTES)
    fun `test superposition kotlin provider integration`() {
        // Run the main provider test
        Main().run()
    }
}
