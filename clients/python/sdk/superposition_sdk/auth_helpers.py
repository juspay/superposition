"""
Authentication helpers for Superposition SDK.

This module provides Bearer Token authentication implementation for the Smithy-generated
Python SDK. This is a temporary workaround until smithy-python codegen adds built-in
auth support (similar to Rust and TypeScript SDKs).

TODO: Remove this once smithy-python 1.0+ provides built-in HTTP auth schemes.
"""

from dataclasses import dataclass
from typing import Any, Dict

from smithy_core.aio.interfaces.identity import IdentityResolver
from smithy_core.interfaces.identity import Identity, IdentityProperties
from smithy_http import Field, Fields
from smithy_http.aio.interfaces.auth import (
    HTTPAuthOption,
    HTTPAuthScheme,
    HTTPSigner,
)

from .auth import HTTPAuthParams, HTTPAuthSchemeResolver
from .config import Config


@dataclass
class BearerTokenIdentity(Identity):
    """Identity containing a bearer token."""

    token: str


class BearerTokenSigner(HTTPSigner[BearerTokenIdentity, Dict[str, Any]]):
    """Signer that adds bearer token to Authorization header."""

    async def sign(
        self,
        *,
        http_request,
        identity: BearerTokenIdentity,
        signing_properties: Dict[str, Any],
    ):
        """Add bearer token to the Authorization header."""
        http_request.fields.extend(
            Fields([Field(name="Authorization", values=[f"Bearer {identity.token}"])])
        )
        return http_request


class BearerTokenResolver(
    IdentityResolver[BearerTokenIdentity, IdentityProperties]
):
    """Resolver that provides the bearer token identity."""

    def __init__(self, token: str):
        if not token or not token.strip():
            raise ValueError("Bearer token cannot be None or empty")
        self._token = token

    async def get_identity(
        self, *, identity_properties: IdentityProperties
    ) -> BearerTokenIdentity:
        """Return the bearer token identity."""
        return BearerTokenIdentity(token=self._token)


@dataclass
class BearerAuthScheme(
    HTTPAuthScheme[
        BearerTokenIdentity, Config, IdentityProperties, Dict[str, Any]
    ]
):
    """HTTP Bearer authentication scheme."""

    scheme_id: str = "smithy.api#httpBearerAuth"
    signer: HTTPSigner = None
    _token: str = None

    def __init__(self, token: str):
        if not token or not token.strip():
            raise ValueError("Bearer token cannot be None or empty")
        self.scheme_id = "smithy.api#httpBearerAuth"
        self._token = token
        self.signer = BearerTokenSigner()

    def identity_resolver(
        self, *, config: Config
    ) -> IdentityResolver[BearerTokenIdentity, IdentityProperties]:
        """Return the identity resolver for this auth scheme."""
        return BearerTokenResolver(self._token)


class BearerAuthSchemeResolver(HTTPAuthSchemeResolver):
    """Auth scheme resolver that returns bearer auth option."""

    def resolve_auth_scheme(
        self, auth_parameters: HTTPAuthParams
    ) -> list[HTTPAuthOption]:
        """Return bearer auth as the authentication option."""
        return [
            HTTPAuthOption(
                scheme_id="smithy.api#httpBearerAuth",
                identity_properties={},
                signer_properties={},
            )
        ]


def create_bearer_auth_config(endpoint: str, token: str) -> Config:
    """
    Convenience function to create a Config with bearer token authentication.

    Args:
        endpoint: The API endpoint URL
        token: The bearer token for authentication

    Returns:
        Config object configured with bearer token authentication

    Example:
        >>> from superposition_sdk.auth_helpers import create_bearer_auth_config
        >>> from superposition_sdk.client import Superposition
        >>>
        >>> config = create_bearer_auth_config(
        ...     endpoint="https://api.example.com",
        ...     token="your-token-here"
        ... )
        >>> client = Superposition(config)
    """
    bearer_scheme = BearerAuthScheme(token=token)
    auth_resolver = BearerAuthSchemeResolver()

    return Config(
        endpoint_uri=endpoint,
        http_auth_scheme_resolver=auth_resolver,
        http_auth_schemes={"smithy.api#httpBearerAuth": bearer_scheme},
    )


# Basic Auth implementation (similar pattern)
@dataclass
class BasicAuthIdentity(Identity):
    """Identity containing username and password for basic auth."""

    username: str
    password: str


class BasicAuthSigner(HTTPSigner[BasicAuthIdentity, Dict[str, Any]]):
    """Signer that adds basic auth to Authorization header."""

    async def sign(
        self,
        *,
        http_request,
        identity: BasicAuthIdentity,
        signing_properties: Dict[str, Any],
    ):
        """Add basic auth to the Authorization header."""
        import base64

        credentials = f"{identity.username}:{identity.password}"
        encoded = base64.b64encode(credentials.encode()).decode()
        http_request.fields.extend(
            Fields([Field(name="Authorization", values=[f"Basic {encoded}"])])
        )
        return http_request


class BasicAuthResolver(IdentityResolver[BasicAuthIdentity, IdentityProperties]):
    """Resolver that provides the basic auth identity."""

    def __init__(self, username: str, password: str):
        if not username or not username.strip():
            raise ValueError("Username cannot be None or empty")
        if not password or not password.strip():
            raise ValueError("Password cannot be None or empty")
        self._username = username
        self._password = password

    async def get_identity(
        self, *, identity_properties: IdentityProperties
    ) -> BasicAuthIdentity:
        """Return the basic auth identity."""
        return BasicAuthIdentity(username=self._username, password=self._password)


@dataclass
class BasicAuthScheme(
    HTTPAuthScheme[BasicAuthIdentity, Config, IdentityProperties, Dict[str, Any]]
):
    """HTTP Basic authentication scheme."""

    scheme_id: str = "smithy.api#httpBasicAuth"
    signer: HTTPSigner = None
    _username: str = None
    _password: str = None

    def __init__(self, username: str, password: str):
        if not username or not username.strip():
            raise ValueError("Username cannot be None or empty")
        if not password or not password.strip():
            raise ValueError("Password cannot be None or empty")
        self.scheme_id = "smithy.api#httpBasicAuth"
        self._username = username
        self._password = password
        self.signer = BasicAuthSigner()

    def identity_resolver(
        self, *, config: Config
    ) -> IdentityResolver[BasicAuthIdentity, IdentityProperties]:
        """Return the identity resolver for this auth scheme."""
        return BasicAuthResolver(self._username, self._password)


class BasicAuthSchemeResolver(HTTPAuthSchemeResolver):
    """Auth scheme resolver that returns basic auth option."""

    def resolve_auth_scheme(
        self, auth_parameters: HTTPAuthParams
    ) -> list[HTTPAuthOption]:
        """Return basic auth as the authentication option."""
        return [
            HTTPAuthOption(
                scheme_id="smithy.api#httpBasicAuth",
                identity_properties={},
                signer_properties={},
            )
        ]


def create_basic_auth_config(endpoint: str, username: str, password: str) -> Config:
    """
    Convenience function to create a Config with basic authentication.

    Args:
        endpoint: The API endpoint URL
        username: The username for basic auth
        password: The password for basic auth

    Returns:
        Config object configured with basic authentication

    Example:
        >>> from superposition_sdk.auth_helpers import create_basic_auth_config
        >>> from superposition_sdk.client import Superposition
        >>>
        >>> config = create_basic_auth_config(
        ...     endpoint="https://api.example.com",
        ...     username="user",
        ...     password="pass"
        ... )
        >>> client = Superposition(config)
    """
    basic_scheme = BasicAuthScheme(username=username, password=password)
    auth_resolver = BasicAuthSchemeResolver()

    return Config(
        endpoint_uri=endpoint,
        http_auth_scheme_resolver=auth_resolver,
        http_auth_schemes={"smithy.api#httpBasicAuth": basic_scheme},
    )
