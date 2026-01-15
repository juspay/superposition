// Build script to automatically transpile Rust API definitions to Smithy IDL
// using Rusty Smith
//
// This script runs during cargo build and generates .smithy files from
// your Rust API definitions.

use std::env;
use std::fs;
use std::path::Path;

fn main() {
    println!("cargo:rerun-if-changed=src/main.rs");

    let out_dir = env::var("OUT_DIR").unwrap();
    let smithy_output_path = Path::new(&out_dir).join("generated.smithy");

    // In a real implementation, you would:
    // 1. Collect all metadata from your API definitions
    // 2. Build a SmithyRegistry
    // 3. Call generate_smithy_idl() to produce the Smithy code
    // 4. Write it to a file

    let example_smithy = r#"
$version: "2.0"

namespace com.example.blog

@documentation("Blog service for managing posts and comments")
@restJson1
service BlogService {
    version: "2024-01-01"
    resources: [
        PostResource
    ]
    operations: [
        CreatePost
        GetPost
        ListPosts
        UpdatePost
        DeletePost
    ]
}

@documentation("Represents a blog post in the system")
structure Post {
    @required
    @documentation("Unique identifier for the post")
    id: String

    @required
    @documentation("Post title")
    title: String

    @required
    @documentation("Post content in markdown format")
    content: String

    @documentation("Current publication status")
    status: PostStatus

    @required
    @documentation("Post author ID")
    author_id: String

    @documentation("Post tags for categorization")
    tags: StringList

    @documentation("Timestamp when the post was created")
    created_at: String

    @documentation("Timestamp when the post was last updated")
    updated_at: String
}

@documentation("Publication status of a blog post")
enum PostStatus {
    Draft = "draft"
    Published = "published"
    Archived = "archived"
}

list StringList {
    member: String
}

@documentation("Request structure for creating a new post")
structure CreatePostRequest {
    @required
    @documentation("Title of the new post")
    title: String

    @required
    @documentation("Content of the new post")
    content: String

    @documentation("Optional tags")
    tags: StringList
}

@documentation("Response structure after creating a post")
structure CreatePostResponse {
    @required
    @documentation("The newly created post")
    post: Post
}

@documentation("Creates a new blog post")
@http(method: "POST", uri: "/posts")
@tags(["Blog Management"])
operation CreatePost {
    input := {
        @httpHeader("x-api-key")
        api_key: String

        @httpPayload
        body: CreatePostRequest
    }

    output := {
        @httpPayload
        @required
        response: CreatePostResponse
    }
}

@documentation("Retrieves a specific post by ID")
@http(method: "GET", uri: "/posts/{id}")
@tags(["Blog Management"])
@readonly
operation GetPost {
    input := {
        @httpLabel
        @required
        id: String

        @httpHeader("x-api-key")
        api_key: String
    }

    output := {
        @httpPayload
        @required
        post: Post
    }
}

@documentation("Lists all posts with optional filtering")
@http(method: "GET", uri: "/posts")
@tags(["Blog Management"])
@readonly
operation ListPosts {
    input := {
        @httpQuery("status")
        status: PostStatus

        @httpQuery("author_id")
        author_id: String

        @httpQuery("page")
        page: Integer

        @httpQuery("limit")
        limit: Integer

        @httpHeader("x-api-key")
        api_key: String
    }

    output := {
        @httpPayload
        @required
        posts: PostList

        @httpHeader("x-total-count")
        total_count: Integer
    }
}

list PostList {
    member: Post
}

@documentation("Updates an existing post")
@http(method: "PUT", uri: "/posts/{id}")
@tags(["Blog Management"])
@idempotent
operation UpdatePost {
    input := {
        @httpLabel
        @required
        id: String

        @httpHeader("x-api-key")
        api_key: String

        @httpPayload
        body: CreatePostRequest
    }

    output := {
        @httpPayload
        @required
        post: Post
    }
}

@documentation("Deletes a post")
@http(method: "DELETE", uri: "/posts/{id}")
@tags(["Blog Management"])
@idempotent
operation DeletePost {
    input := {
        @httpLabel
        @required
        id: String

        @httpHeader("x-api-key")
        api_key: String
    }

    output := {}
}

@documentation("Blog post resource")
resource PostResource {
    identifiers: {
        id: String
    }
    properties: {
        title: String
        content: String
        status: PostStatus
    }
    read: GetPost
    list: ListPosts
    create: CreatePost
    update: UpdatePost
    delete: DeletePost
}
"#;

    fs::write(&smithy_output_path, example_smithy).expect("Failed to write Smithy file");

    println!(
        "Generated Smithy IDL at: {}",
        smithy_output_path.display()
    );
}
