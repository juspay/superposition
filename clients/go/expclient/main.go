package main

/*
#cgo CFLAGS: -I.
#cgo LDFLAGS: -L. -lexperimentation_client
#include "libexperimentation_client.h"
*/
import "C"
import (
	"fmt"
	"runtime"
	"time"
)

// Define ExperimentationClient structure
type ExperimentationClient struct {
	tenant           string
	pollingFrequency int
	cacHostName      string
}

// Constructor
func NewExperimentationClient(tenant string, pollingFrequency int, hostName string) *ExperimentationClient {
	return &ExperimentationClient{
		tenant:           tenant,
		pollingFrequency: pollingFrequency,
		cacHostName:      hostName,
	}
}

// Methods
func (ec *ExperimentationClient) CreateNewExperimentationClient() int {
	respCode := C.expt_new_client(C.CString(ec.tenant), C.ulong(ec.pollingFrequency), C.CString(ec.cacHostName))
	if respCode == 1 {
		errorMessage := C.GoString(C.expt_last_error_message())
		fmt.Printf("Error occurred while creating new experimentation client: %s\n", errorMessage)
		return int(respCode)
	}
	return int(respCode)
}

func (ec *ExperimentationClient) GetExperimentationClient() *C.Arc_Client {
	return C.expt_get_client(C.CString(ec.tenant))
}

func (ec *ExperimentationClient) GetRunningExperiments() string {
	clientPtr := ec.GetExperimentationClient()
	return C.GoString(C.expt_get_running_experiments(clientPtr))
}

func (ec *ExperimentationClient) FreeString(str string) {
	C.expt_free_string(C.CString(str))
}

func (ec *ExperimentationClient) StartExperimentationPollingUpdate() {
	if runtime.GOARCH == "amd64" {
		go func() {
			for {
				time.Sleep(time.Duration(ec.pollingFrequency) * time.Second)
				C.expt_start_polling_update(C.CString(ec.tenant))
			}
		}()
	} else {
		C.expt_start_polling_update(C.CString(ec.tenant))
	}
}

func (ec *ExperimentationClient) GetExperimentationLastErrorLength() int {
	return int(C.expt_last_error_length())
}

func (ec *ExperimentationClient) FreeExperimentationClient() {
	clientPtr := ec.GetExperimentationClient()
	C.expt_free_client(clientPtr)
}

func (ec *ExperimentationClient) GetFilteredSatisfiedExperiments(context, filterPrefix string) string {
	clientPtr := ec.GetExperimentationClient()
	return C.GoString(C.expt_get_filtered_satisfied_experiments(clientPtr, C.CString(context), C.CString(filterPrefix)))
}

func (ec *ExperimentationClient) GetApplicableVariant(context string, toss int) string {
	clientPtr := ec.GetExperimentationClient()
	return C.GoString(C.expt_get_applicable_variant(clientPtr, C.CString(context), C.short(toss)))
}

func (ec *ExperimentationClient) GetSatisfiedExperiments(context string, filterPrefix string) string {
	clientPtr := ec.GetExperimentationClient()
	return C.GoString(C.expt_get_satisfied_experiments(clientPtr, C.CString(context), C.CString(filterPrefix)))
}

func main() {
	// Example usage
	client := NewExperimentationClient("dev", 10, "http://localhost:8080")
	client.CreateNewExperimentationClient()
	fmt.Println("Running experiments:", client.GetRunningExperiments())
}
