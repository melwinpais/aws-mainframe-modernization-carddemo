# CardDemo Frontend Application

Modern Vue.js 3 frontend application for the CardDemo credit card management system.

## Technology Stack

- **Framework**: Vue.js 3
- **Build Tool**: Vite
- **State Management**: Pinia
- **Routing**: Vue Router
- **HTTP Client**: Axios

## Prerequisites

- Node.js 18+ and npm/yarn
- Backend API running on http://localhost:8080

## Getting Started

### Installation

```bash
npm install
```

### Development

Start the development server:

```bash
npm run dev
```

The application will be available at `http://localhost:3000`

### Build for Production

```bash
npm run build
```

The production-ready files will be in the `dist/` directory.

### Preview Production Build

```bash
npm run preview
```

## Project Structure

```
src/
├── assets/          # Static assets (CSS, images)
├── components/      # Reusable Vue components
├── router/          # Vue Router configuration
├── services/        # API service modules
├── stores/          # Pinia stores for state management
└── views/           # Page components
```

## Environment Variables

Create `.env.local` file for local development:

```
VITE_API_BASE_URL=http://localhost:8080/api
VITE_APP_TITLE=CardDemo - Development
```

## Available Scripts

- `npm run dev` - Start development server
- `npm run build` - Build for production
- `npm run preview` - Preview production build
- `npm run lint` - Lint and fix files
- `npm run format` - Format code with Prettier

## Features

- User authentication with JWT
- Role-based access control (Admin/User)
- Account management
- Card management
- Transaction viewing
- Bill payment
- Report generation
- User management (Admin only)

## API Integration

The frontend communicates with the backend API through Axios. The base URL is configured via environment variables.

API client configuration is in `src/services/api.js` with:
- Request interceptor for JWT token injection
- Response interceptor for error handling
- Automatic token refresh handling

## State Management

Pinia stores are used for:
- Authentication state (`stores/auth.js`)
- Account state (to be added)
- Card state (to be added)
- Transaction state (to be added)

## Routing

Vue Router handles navigation with:
- Authentication guards
- Role-based access control
- Lazy-loaded route components

## Development Guidelines

1. Use composition API for new components
2. Follow Vue.js style guide
3. Keep components small and focused
4. Use Pinia for shared state
5. Handle errors gracefully with user-friendly messages

## License

Copyright AWS - CardDemo Modernization Project
