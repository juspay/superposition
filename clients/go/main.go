package main

import (
	"fmt"
	"goclients/cacclient"
	"goclients/expclient"
)

func main() {
	// Example usage
	tenant := "dev"
	pollingFrequency := 1
	cachostName := "http://localhost:8080"

	client, error := cacclient.NewCacClient(tenant, pollingFrequency, cachostName)
	if error != nil {
		fmt.Println(error)
	}

	fmt.Println("\n------------Configs----------------------------\n")

	fmt.Println("Default Configs => ", client.GetConfig(nil, nil))
	fmt.Println("Resolved Config => ", client.GetResolvedConfig(map[string]string{}, nil, cacclient.MERGE))
	fmt.Println("Default Config => ", client.GetDefaultConfig(&[]string{}))

	fmt.Println("\n------------Experiments----------------------------\n")

	expClient, error1 := expclient.NewExperimentationClient(tenant, pollingFrequency, cachostName)
	if error1 != nil {
		fmt.Println(error1)
	}
	fmt.Println("Running experiments => ", expClient.GetRunningExperiments())
	fmt.Println("Filtered Satisfied Experiments => ", expClient.GetFilteredSatisfiedExperiments(map[string]string{}, &[]string{}))
	fmt.Println("Applicable Variant => ", expClient.GetApplicableVariant(map[string]string{}, 1))
	fmt.Println("Satisfied Experiments => ", expClient.GetSatisfiedExperiments(map[string]string{}, &[]string{}))
}
