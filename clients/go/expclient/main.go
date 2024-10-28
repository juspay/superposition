package expclient

/*
#include "libexperimentation_client.h"
*/
import "C"
import (
	"encoding/json"
	"errors"
	"fmt"
	"strings"
)

// Define ExperimentationClient structure
type ExperimentationClient struct {
	tenant           string
	pollingFrequency int
	cacHostName      string
	delimiter        string
}

// Constructor
func NewExperimentationClient(tenantName string, pollingFrequency int, cacHostName string) (*ExperimentationClient, error) {
	if tenantName == "" {
		return nil, errors.New("tenantName cannot be Empty")
	}
	if cacHostName == "" {
		return nil, errors.New("cacHostName cannot be Empty")
	}
	client := &ExperimentationClient{
		tenant:           tenantName,
		pollingFrequency: pollingFrequency,
		cacHostName:      cacHostName,
		delimiter:        ",",
	}
	respCode := C.expt_new_client(C.CString(tenantName), C.ulong(pollingFrequency), C.CString(cacHostName))
	if respCode == 1 {
		errorMessage := client.GetLastErrorMessage()
		fmt.Printf("Error occurred while creating new experimentation client: %s\n", errorMessage)
	}
	return client, nil
}

func (ec *ExperimentationClient) GetLastErrorMessage() string {
	message := C.expt_last_error_message()
	if message == nil {
		return ""
	}
	return C.GoString(message)
}

func (ec *ExperimentationClient) GetClient() *C.Arc_Client {
	return C.expt_get_client(C.CString(ec.tenant))
}

func (ec *ExperimentationClient) GetRunningExperiments() string {
	clientPtr := ec.GetClient()
	resp := C.expt_get_running_experiments(clientPtr)
	if resp == nil {
		return ec.GetLastErrorMessage()
	}
	return C.GoString(resp)
}

func (ec *ExperimentationClient) FreeString(str string) {
	C.expt_free_string(C.CString(str))
}

func (ec *ExperimentationClient) StartPollingUpdate() {
	go C.expt_start_polling_update(C.CString(ec.tenant))
}

func (ec *ExperimentationClient) GetLastErrorLength() int {
	return int(C.expt_last_error_length())
}

func (ec *ExperimentationClient) FreeClient() {
	clientPtr := ec.GetClient()
	C.expt_free_client(clientPtr)
}

func (ec *ExperimentationClient) GetFilteredSatisfiedExperiments(context map[string]string, filterPrefix *[]string) string {
	clientPtr := ec.GetClient()
	strContext, err := json.Marshal(context)
	if err != nil {
		fmt.Println("Failed to covert json to string")
		return ""
	}
	var strFilterPrefix *string

	if filterPrefix != nil {
		val := strings.Join(*filterPrefix, ec.delimiter)
		strFilterPrefix = &val
	}

	var fk *C.char
	if strFilterPrefix != nil {
		fk = C.CString(*strFilterPrefix)
	} else {
		fk = nil
	}
	resp := C.expt_get_filtered_satisfied_experiments(clientPtr, C.CString(string(strContext)), fk)
	if resp == nil {
		return ec.GetLastErrorMessage()
	}

	return C.GoString(resp)
}

func (ec *ExperimentationClient) GetApplicableVariant(context map[string]string, toss int) string {
	clientPtr := ec.GetClient()
	strContext, err := json.Marshal(context)
	if err != nil {
		fmt.Println("Failed to covert json to string")
		return ""
	}
	resp := C.expt_get_applicable_variant(clientPtr, C.CString(string(strContext)), C.short(toss))
	if resp == nil {
		return ec.GetLastErrorMessage()
	}
	return C.GoString(resp)
}

func (ec *ExperimentationClient) GetSatisfiedExperiments(context map[string]string, filterPrefix *[]string) string {
	clientPtr := ec.GetClient()
	strContext, err := json.Marshal(context)
	var strFilterPrefix *string

	if filterPrefix != nil {
		val := strings.Join(*filterPrefix, ec.delimiter)
		strFilterPrefix = &val
	}

	var fk *C.char
	if strFilterPrefix != nil {
		fk = C.CString(*strFilterPrefix)
	} else {
		fk = nil
	}

	if err != nil {
		fmt.Println("Failed to covert json to string")
		return ""
	}
	resp := C.expt_get_satisfied_experiments(clientPtr, C.CString(string(strContext)), fk)
	if resp == nil {
		return ec.GetLastErrorMessage()
	}
	return C.GoString(resp)
}
