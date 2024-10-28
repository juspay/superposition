package cacclient

/*
#include "libcac_client.h"
*/
import "C"
import (
	"encoding/json"
	"errors"
	"fmt"
	"strings"
	"unsafe"
)

type MergeStrategy string

const (
	MERGE   MergeStrategy = "MERGE"
	REPLACE MergeStrategy = "REPLACE"
)

// CacClient struct
type CacClient struct {
	tenant           string
	pollingFrequency int
	cacHostName      string
	delimiter        string
}

// NewCacClient creates a new CacClient
func NewCacClient(tenantName string, pollingFrequency int, cacHostName string) (*CacClient, error) {
	if tenantName == "" {
		return nil, errors.New("tenantName cannot be Empty")
	}
	if cacHostName == "" {
		return nil, errors.New("cacHostName cannot be Empty")
	}

	client := &CacClient{tenant: tenantName, pollingFrequency: pollingFrequency, cacHostName: cacHostName, delimiter: ","}
	resp := C.cac_new_client(C.CString(tenantName), C.ulong(pollingFrequency), C.CString(cacHostName))
	if resp == 1 {
		errorMessage := client.GetLastErrorMessage()
		fmt.Printf("Some Error Occur while creating new client: %s\n", errorMessage)
	}
	return client, nil
}

// NewCacClientWithCacheProperties creates a new CacClient with custom cache value
func NewCacClientWithCacheProperties(tenantName string, pollingFrequency int, cacHostName string, cacheMaxCapacity int, cacheTTL int, cacheTTI int) (*CacClient, error) {
	if tenantName == "" {
		return nil, errors.New("tenantName cannot be Empty")
	}
	if cacHostName == "" {
		return nil, errors.New("cacHostName cannot be Empty")
	}
	
	client := &CacClient{tenant: tenantName, pollingFrequency: pollingFrequency, cacHostName: cacHostName, delimiter: ","}
	resp := C.cac_new_client_with_cache_properties(C.CString(tenantName), C.ulong(pollingFrequency), C.CString(cacHostName), cacheMaxCapacity, cacheTTL, cacheTTI)
	if resp == 1 {
		errorMessage := client.GetLastErrorMessage()
		fmt.Printf("Some Error Occur while creating new client: %s\n", errorMessage)
	}
	return client, nil
}

// GetLastErrorMessage gets the last error message from the C library
func (c *CacClient) GetLastErrorMessage() string {
	message := C.cac_last_error_message()
	if message == nil {
		return ""
	}
	return C.GoString(message)
}

// GetLastErrorLength gets the length of the last error message
func (c *CacClient) GetLastErrorLength() int {
	return int(C.cac_last_error_length())
}

// GetClient gets the client pointer from the C library
func (c *CacClient) GetClient() *C.Arc_Client {
	return C.cac_get_client(C.CString(c.tenant))
}

// StartCacPollingUpdate starts polling updates in a separate goroutine
func (c *CacClient) StartPollingUpdate() {
	tenant := C.CString(c.tenant)
	go C.cac_start_polling_update(tenant)
}

// GetConfig gets the configuration from the C library
func (c *CacClient) GetConfig(filterQuery *map[string]string, filterPrefix *[]string) string {
	var strFilterPrefix, strFilterQuery *string

	if filterPrefix != nil {
		val := strings.Join(*filterPrefix, c.delimiter)
		strFilterPrefix = &val
	}

	if filterQuery != nil {
		byteVal, err := json.Marshal(filterQuery)
		if err != nil {
			fmt.Println("Failed to covert json to string")
			return ""
		}
		strVal := string(byteVal)
		strFilterQuery = &strVal
	}
	clientPtr := c.GetClient()
	var fp, fq *C.char

	if strFilterPrefix != nil {
		fp = C.CString(*strFilterPrefix)
	} else {
		fp = nil
	}

	if strFilterQuery != nil {
		fq = C.CString(*strFilterQuery)
	} else {
		fq = nil
	}
	resp := C.cac_get_config(clientPtr, fq, fp)
	if resp == nil {
		return c.GetLastErrorMessage()
	}
	return C.GoString(resp)

}

// FreeClient frees the client in the C library
func (c *CacClient) FreeClient(clientPtr string) {
	ptr := c.GetClient()
	C.cac_free_client(ptr)
}

// FreeCacString frees a string in the C library
func (c *CacClient) FreeString(s string) {
	cs := C.CString(s)
	defer C.free(unsafe.Pointer(cs))
	C.cac_free_string(cs)
}

// GetLastModified gets the last modified timestamp from the C library
func (c *CacClient) GetLastModified() string {
	clientPtr := c.GetClient()
	return C.GoString(C.cac_get_last_modified(clientPtr))
}

// GetResolvedConfig gets the resolved configuration from the C library
func (c *CacClient) GetResolvedConfig(query map[string]string, filterKeys *[]string, mergeStrategy MergeStrategy) string {

	var strfilterKeys *string

	if filterKeys != nil {
		val := strings.Join(*filterKeys, "|")
		strfilterKeys = &val
	}

	var fk *C.char
	if strfilterKeys != nil {
		fk = C.CString(*strfilterKeys)
	} else {
		fk = nil
	}
	clientPtr := c.GetClient()
	strQuery, err := json.Marshal(query)
	if err != nil {
		fmt.Println("Failed to covert json to string")
		return ""
	}
	q := C.CString(string(strQuery))
	ms := C.CString(string(mergeStrategy))

	resp := C.cac_get_resolved_config(clientPtr, q, fk, ms)
	if resp == nil {
		return c.GetLastErrorMessage()
	}
	return C.GoString(resp)

}

// GetDefaultConfig gets the default configuration from the C library
func (c *CacClient) GetDefaultConfig(filterKeys *[]string) string {
	clientPtr := c.GetClient()
	var strfilterKeys *string

	if filterKeys != nil {
		val := strings.Join(*filterKeys, "|")
		strfilterKeys = &val
	}

	var fk *C.char
	if strfilterKeys != nil {
		fk = C.CString(*strfilterKeys)
	} else {
		fk = nil
	}

	resp := C.cac_get_default_config(clientPtr, fk)
	if resp == nil {
		return c.GetLastErrorMessage()
	}
	return C.GoString(resp)
}
