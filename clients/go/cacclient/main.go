package main

/*
#cgo CFLAGS: -I.
#cgo LDFLAGS: -L. -lcac_client
#include "libcac_client.h"
*/
import "C"
import (
	"fmt"
	"unsafe"
)

// CacClient struct
type CacClient struct {
	tenant           string
	pollingFrequency int
	cacHostName      string
}

// NewCacClient creates a new CacClient
func NewCacClient(tenantName string, pollingFrequency int, cacHostName string) *CacClient {
	return &CacClient{tenant: tenantName, pollingFrequency: pollingFrequency, cacHostName: cacHostName}
}

// GetCacLastErrorMessage gets the last error message from the C library
func (c *CacClient) GetCacLastErrorMessage() string {
	return C.GoString(C.cac_last_error_message())
}

// GetCacLastErrorLength gets the length of the last error message
func (c *CacClient) GetCacLastErrorLength() int {
	return int(C.cac_last_error_length())
}

// GetCacClient gets the client pointer from the C library
func (c *CacClient) GetCacClient() *C.Arc_Client {
	return C.cac_get_client(C.CString(c.tenant))
}

// CreateNewCacClient creates a new client in the C library
func (c *CacClient) CreateNewCacClient() int {
	resp := C.cac_new_client(C.CString(c.tenant), C.ulong(c.pollingFrequency), C.CString(c.cacHostName))
	if resp == 1 {
		errorMessage := c.GetCacLastErrorMessage()
		fmt.Printf("Some Error Occur while creating new client: %s\n", errorMessage)
	}
	return int(resp)
}

// StartCacPollingUpdate starts polling updates in a separate goroutine
func (c *CacClient) StartCacPollingUpdate() {
	tenant := C.CString(c.tenant)
	C.cac_start_polling_update(tenant)
}

// GetCacConfig gets the configuration from the C library
func (c *CacClient) GetCacConfig(filterQuery string, filterPrefix string) string {
	clientPtr := c.GetCacClient()
	fq := C.CString(filterQuery)
	fp := C.CString(filterPrefix)
	return C.GoString(C.cac_get_config(clientPtr, fq, fp))
}

// FreeCacClient frees the client in the C library
func (c *CacClient) FreeCacClient(clientPtr string) {
	ptr := c.GetCacClient()
	C.cac_free_client(ptr)
}

// FreeCacString frees a string in the C library
func (c *CacClient) FreeCacString(s string) {
	cs := C.CString(s)
	defer C.free(unsafe.Pointer(cs))
	C.cac_free_string(cs)
}

// GetLastModified gets the last modified timestamp from the C library
func (c *CacClient) GetLastModified() string {
	clientPtr := c.GetCacClient()
	return C.GoString(C.cac_get_last_modified(clientPtr))
}

// GetResolvedConfig gets the resolved configuration from the C library
func (c *CacClient) GetResolvedConfig(query, filterKeys, mergeStrategy string) string {
	clientPtr := c.GetCacClient()
	q := C.CString(query)
	fk := C.CString(filterKeys)
	ms := C.CString(mergeStrategy)

	return C.GoString(C.cac_get_resolved_config(clientPtr, q, fk, ms))
}

// GetDefaultConfig gets the default configuration from the C library
func (c *CacClient) GetDefaultConfig(filterKeys string) string {
	clientPtr := c.GetCacClient()
	fk := C.CString(filterKeys)
	return C.GoString(C.cac_get_default_config(clientPtr, fk))
}

func main() {
	// Example usage
	client := NewCacClient("dev", 10, "http://localhost:8080")
	client.CreateNewCacClient()
	fmt.Println("Running experiments:", client.GetCacConfig("{}", ""))
}
