# Quick Start Guide

Get up and running with the Superposition admin integration example in 3 steps!

## Step 1: Install Dependencies

```bash
npm install
```

## Step 2: Start Superposition Backend

In a separate terminal, from the repository root:

```bash
make run
```

This starts the backend at http://localhost:8080

## Step 3: Start the Example

```bash
npm start
```

This opens http://localhost:8000 in your browser.

## 🎉 You're Ready!

Explore the examples:

- **Dashboard** - Overview and documentation
- **Configuration** - Full admin interface integration
- **Custom Theme** - Theme switching demo
- **Dynamic Loading** - Performance optimization pattern

## ⚡ Quick Commands

```bash
npm start              # Start development server
npm run dev            # Same as npm start
```

## 🐛 Troubleshooting

### Backend Not Running?

Make sure you've started the Superposition backend:

```bash
# From repository root
make run
```

### Port 8000 Already in Use?

Start on a different port:

```bash
npx http-server -p 3000
```

### Can't Install Dependencies?

Make sure you're in the example directory:

```bash
cd examples/html-integration-example
npm install
```

## 📖 Learn More

See [README.md](README.md) for detailed documentation.
