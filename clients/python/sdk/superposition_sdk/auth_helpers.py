"""
Authentication helpers for Superposition SDK.

This module provides Bearer Token and Basic authentication implementations for the Smithy-generated
Python SDK. This is a temporary workaround until smithy-python codegen adds built-in
auth support (similar to Rust and TypeScript SDKs).

Supported authentication schemes:
    - Bearer Token: HTTP Bearer authentication using bearer tokens
    - Basic Auth: HTTP Basic authentication using username and password

TODO: Remove this once smithy-python 1.0+ provides built-in HTTP auth schemes.
"""

from dataclasses import dataclass
from typing import Any, Dict

from smithy_core.aio.interfaces.identity import IdentityResolver
from smithy_core.interfaces.identity import Identity, IdentityProperties
from smithy_http import Field, Fields
from smithy_http.aio import HTTPRequest
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


class BearerTokenSigner(HTTPSigner[BearerTokenIdentity, IdentityProperties]):
    """Signer that adds bearer token to Authorization header."""

    async def sign(
        self,
        *,
        http_request: HTTPRequest,
        identity: BearerTokenIdentity,
        signing_properties: IdentityProperties,
    ) -> HTTPRequest:
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
    signer: HTTPSigner[BearerTokenIdentity, IdentityProperties] = None
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


def bearer_auth_config(token: str) -> tuple[BearerAuthSchemeResolver, Dict[str, BearerAuthScheme]]:
    """
    Convenience function to get the config for bearer token authentication.

    Args:
        token: The bearer token for authentication

    Returns:
        Tuple of BearerAuthSchemeResolver and dict of BearerAuthSchemes

    Example:
        >>> from superposition_sdk.auth_helpers import bearer_auth_config
        >>> from superposition_sdk.client import Superposition
        >>> from superposition_sdk.config import Config
        >>>
        >>> (resolver, schemes) = bearer_auth_config(token="your-token-here")
        >>> config = Config(
        ...     endpoint_uri="https://api.example.com",
        ...     http_auth_scheme_resolver=resolver,
        ...     http_auth_schemes=schemes
        ... )
        >>> client = Superposition(config)
    """
    bearer_scheme = BearerAuthScheme(token=token)
    auth_resolver = BearerAuthSchemeResolver()

    return auth_resolver, {"smithy.api#httpBearerAuth": bearer_scheme}


# Basic Auth implementation (similar pattern)
@dataclass
class BasicAuthIdentity(Identity):
    """Identity containing username and password for basic auth."""

    username: str
    password: str


class BasicAuthSigner(HTTPSigner[BasicAuthIdentity, IdentityProperties]):
    """Signer that adds basic auth to Authorization header."""

    async def sign(
        self,
        *,
        http_request: HTTPRequest,
        identity: BasicAuthIdentity,
        signing_properties: IdentityProperties,
    ) -> HTTPRequest:
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
    signer: HTTPSigner[BasicAuthIdentity, IdentityProperties] = None
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


def basic_auth_config(username: str, password: str) -> tuple[BasicAuthSchemeResolver, Dict[str, BasicAuthScheme]]:
    """
    Convenience function to get the config for basic authentication.

    Args:
        username: The username for basic auth
        password: The password for basic auth

    Returns:
        Tuple of BasicAuthSchemeResolver and dict of BasicAuthSchemes

    Example:
        >>> from superposition_sdk.auth_helpers import basic_auth_config
        >>> from superposition_sdk.client import Superposition
        >>> from superposition_sdk.config import Config
        >>>
        >>> (resolver, schemes) = basic_auth_config(username="user", password="pass")
        >>> config = Config(
        ...     endpoint_uri="https://api.example.com",
        ...     http_auth_scheme_resolver=resolver,
        ...     http_auth_schemes=schemes
        ... )
        >>> client = Superposition(config)
    """
    basic_scheme = BasicAuthScheme(username=username, password=password)
    auth_resolver = BasicAuthSchemeResolver()

    return auth_resolver, {"smithy.api#httpBasicAuth": basic_scheme}
