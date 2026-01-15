// Example: Using Rusty Smith to define APIs in Rust
//
// This example shows how to define API interfaces in Rust using macros
// and transpile them to Smithy IDL format.

use rusty_smith::{
    smithy_operation, smithy_resource, smithy_service, SmithyEnum, SmithyShape,
};
use std::collections::HashMap;

// Define a service
#[smithy_service(
    namespace = "com.example.blog",
    version = "2024-01-01",
    protocol = "restJson1"
)]
#[derive(Debug)]
pub struct BlogService;

// Define enums
#[derive(SmithyEnum)]
#[smithy(namespace = "com.example.blog")]
pub enum PostStatus {
    #[smithy(value = "draft")]
    Draft,

    #[smithy(value = "published")]
    Published,

    #[smithy(value = "archived")]
    Archived,
}

// Define data structures
/// Represents a blog post in the system
#[derive(SmithyShape)]
#[smithy(namespace = "com.example.blog")]
pub struct Post {
    /// Unique identifier for the post
    #[smithy(required)]
    pub id: String,

    /// Post title
    #[smithy(required)]
    pub title: String,

    /// Post content in markdown format
    #[smithy(required)]
    pub content: String,

    /// Current publication status
    pub status: PostStatus,

    /// Post author ID
    #[smithy(required)]
    pub author_id: String,

    /// Post tags for categorization
    pub tags: Vec<String>,

    /// Timestamp when the post was created
    pub created_at: String,

    /// Timestamp when the post was last updated
    pub updated_at: Option<String>,
}

/// Request structure for creating a new post
#[derive(SmithyShape)]
#[smithy(namespace = "com.example.blog")]
pub struct CreatePostRequest {
    /// Title of the new post
    #[smithy(required)]
    pub title: String,

    /// Content of the new post
    #[smithy(required)]
    pub content: String,

    /// Optional tags
    pub tags: Option<Vec<String>>,
}

/// Response structure after creating a post
#[derive(SmithyShape)]
#[smithy(namespace = "com.example.blog")]
pub struct CreatePostResponse {
    /// The newly created post
    #[smithy(required)]
    pub post: Post,
}

// Define operations
/// Creates a new blog post
#[smithy_operation(
    http_method = "POST",
    uri = "/posts",
    tags = "Blog Management"
)]
pub struct CreatePost {
    #[smithy(http_header = "x-api-key")]
    pub api_key: String,

    #[smithy(http_payload)]
    pub body: CreatePostRequest,
}

/// Retrieves a specific post by ID
#[smithy_operation(
    http_method = "GET",
    uri = "/posts/{id}",
    tags = "Blog Management",
    readonly
)]
pub struct GetPost {
    #[smithy(http_label)]
    pub id: String,

    #[smithy(http_header = "x-api-key")]
    pub api_key: String,
}

/// Lists all posts with optional filtering
#[smithy_operation(
    http_method = "GET",
    uri = "/posts",
    tags = "Blog Management",
    readonly
)]
pub struct ListPosts {
    #[smithy(http_query = "status")]
    pub status: Option<PostStatus>,

    #[smithy(http_query = "author_id")]
    pub author_id: Option<String>,

    #[smithy(http_query = "page")]
    pub page: Option<i32>,

    #[smithy(http_query = "limit")]
    pub limit: Option<i32>,

    #[smithy(http_header = "x-api-key")]
    pub api_key: String,
}

/// Updates an existing post
#[smithy_operation(
    http_method = "PUT",
    uri = "/posts/{id}",
    tags = "Blog Management",
    idempotent
)]
pub struct UpdatePost {
    #[smithy(http_label)]
    pub id: String,

    #[smithy(http_header = "x-api-key")]
    pub api_key: String,

    #[smithy(http_payload)]
    pub body: CreatePostRequest,
}

/// Deletes a post
#[smithy_operation(
    http_method = "DELETE",
    uri = "/posts/{id}",
    tags = "Blog Management",
    idempotent
)]
pub struct DeletePost {
    #[smithy(http_label)]
    pub id: String,

    #[smithy(http_header = "x-api-key")]
    pub api_key: String,
}

// Define a resource
#[smithy_resource(
    identifiers = "id: String",
    read = "GetPost",
    list = "ListPosts",
    create = "CreatePost",
    update = "UpdatePost",
    delete = "DeletePost"
)]
pub struct PostResource {
    pub id: String,
    pub title: String,
    pub content: String,
    pub status: PostStatus,
}

fn main() {
    println!("Rusty Smith Example");
    println!("===================\n");

    println!("This example demonstrates how to define APIs in Rust that can be");
    println!("transpiled to Smithy IDL format.\n");

    println!("Key features:");
    println!("  • Service definitions with namespaces and protocols");
    println!("  • Resource definitions with CRUD operations");
    println!("  • Operation definitions with HTTP bindings");
    println!("  • Shape definitions (structures) with documentation");
    println!("  • Enum definitions with custom values");
    println!("  • HTTP bindings (headers, query params, path labels, payloads)");
    println!("  • Automatic type mapping (String, Integer, Boolean, Lists, Maps)\n");

    println!("To transpile your Rust API definitions to Smithy:");
    println!("  1. Use the provided macros to annotate your types");
    println!("  2. Register all definitions with SmithyRegistry");
    println!("  3. Call generate_smithy_idl() to produce .smithy files\n");

    println!("Example output location: ./generated/blog.smithy");
}
