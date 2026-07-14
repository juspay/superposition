"""
Typed errors raised by the Superposition provider.

Mirrors the Rust `SuperpositionError` enum and the Java `SuperpositionError` exception, so a
failure means the same thing in every client.
"""

from enum import Enum
from typing import Optional


class ErrorCode(Enum):
    """The kind of failure a SuperpositionError represents."""

    CONFIG_ERROR = "CONFIG_ERROR"
    NETWORK_ERROR = "NETWORK_ERROR"
    SERIALIZATION_ERROR = "SERIALIZATION_ERROR"
    PROVIDER_ERROR = "PROVIDER_ERROR"
    DATA_SOURCE_ERROR = "DATA_SOURCE_ERROR"
    REFRESH_ERROR = "REFRESH_ERROR"


class SuperpositionError(Exception):
    """Raised when a provider or data source operation fails."""

    def __init__(self, code: ErrorCode, message: str, cause: Optional[BaseException] = None):
        super().__init__(message)
        self.code = code
        self.message = message
        self.cause = cause
        if cause is not None:
            self.__cause__ = cause

    def __str__(self) -> str:
        return f"{self.code.value}: {self.message}"

    # --- Factories, one per variant ---

    @staticmethod
    def config_error(message: str, cause: Optional[BaseException] = None) -> "SuperpositionError":
        return SuperpositionError(ErrorCode.CONFIG_ERROR, message, cause)

    @staticmethod
    def network_error(message: str, cause: Optional[BaseException] = None) -> "SuperpositionError":
        return SuperpositionError(ErrorCode.NETWORK_ERROR, message, cause)

    @staticmethod
    def serialization_error(message: str, cause: Optional[BaseException] = None) -> "SuperpositionError":
        return SuperpositionError(ErrorCode.SERIALIZATION_ERROR, message, cause)

    @staticmethod
    def provider_error(message: str, cause: Optional[BaseException] = None) -> "SuperpositionError":
        return SuperpositionError(ErrorCode.PROVIDER_ERROR, message, cause)

    @staticmethod
    def data_source_error(message: str, cause: Optional[BaseException] = None) -> "SuperpositionError":
        return SuperpositionError(ErrorCode.DATA_SOURCE_ERROR, message, cause)

    @staticmethod
    def refresh_error(message: str, cause: Optional[BaseException] = None) -> "SuperpositionError":
        return SuperpositionError(ErrorCode.REFRESH_ERROR, message, cause)
