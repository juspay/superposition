{{/*
Expand the name of the chart.
*/}}
{{- define "superposition.name" -}}
{{- default .Chart.Name .Values.nameOverride | trunc 63 | trimSuffix "-" }}
{{- end }}

{{/*
Create a default fully qualified app name.
We truncate at 63 chars because some Kubernetes name fields are limited to this (by the DNS naming spec).
If release name contains chart name it will be used as a full name.
*/}}
{{- define "superposition.fullname" -}}
{{- if .Values.fullnameOverride }}
{{- .Values.fullnameOverride | trunc 63 | trimSuffix "-" }}
{{- else }}
{{- $name := default .Chart.Name .Values.nameOverride }}
{{- if contains $name .Release.Name }}
{{- .Release.Name | trunc 63 | trimSuffix "-" }}
{{- else }}
{{- printf "%s-%s" .Release.Name $name | trunc 63 | trimSuffix "-" }}
{{- end }}
{{- end }}
{{- end }}

{{/*
Create chart name and version as used by the chart label.
*/}}
{{- define "superposition.chart" -}}
{{- printf "%s-%s" .Chart.Name .Chart.Version | replace "+" "_" | trunc 63 | trimSuffix "-" }}
{{- end }}

{{/*
Common labels
*/}}
{{- define "superposition.labels" -}}
helm.sh/chart: {{ include "superposition.chart" . }}
{{ include "superposition.selectorLabels" . }}
{{- if .Chart.AppVersion }}
app.kubernetes.io/version: {{ .Chart.AppVersion | quote }}
{{- end }}
app.kubernetes.io/managed-by: {{ .Release.Service }}
{{- end }}

{{/*
Selector labels
*/}}
{{- define "superposition.selectorLabels" -}}
app.kubernetes.io/name: {{ include "superposition.name" . }}
app.kubernetes.io/instance: {{ .Release.Name }}
{{- end }}

{{/*
Create the name of the service account to use
*/}}
{{- define "superposition.serviceAccountName" -}}
{{- if .Values.serviceAccount.create }}
{{- default (include "superposition.fullname" .) .Values.serviceAccount.name }}
{{- else }}
{{- default "default" .Values.serviceAccount.name }}
{{- end }}
{{- end }}

{{/*
Convert configs map to ConfigMap data with capitalized keys
*/}}
{{- define "superposition.configsToData" -}}
{{- range $key, $value := .Values.configs }}
{{- if $value }}
{{ $key | upper }}: {{ $value | quote }}
{{- end }}
{{- end }}
{{- end }}

{{/*
Convert secrets map to Secret data with capitalized keys and base64 encoding
*/}}
{{- define "superposition.secretsToData" -}}
{{- range $key, $value := .Values.secrets }}
{{- if $value }}
{{ $key | upper }}: {{ $value | b64enc }}
{{- end }}
{{- end }}
{{- end }}

{{/*
Get the full image name
*/}}
{{- define "superposition.image" -}}
{{- $registry := .Values.global.imageRegistry | default .Values.image.registry }}
{{- $repository := .Values.image.repository }}
{{- $tag := .Values.image.tag | default .Chart.AppVersion }}
{{- printf "%s/%s:%s" $registry $repository $tag }}
{{- end }}
