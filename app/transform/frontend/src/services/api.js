import axios from 'axios'
import { useAuthStore } from '@/stores/auth'
import router from '@/router'

const api = axios.create({
  baseURL: import.meta.env.VITE_API_BASE_URL || '/api',
  timeout: 10000,
  headers: {
    'Content-Type': 'application/json'
  }
})

// Request interceptor to add JWT token
api.interceptors.request.use(
  (config) => {
    const authStore = useAuthStore()
    if (authStore.token) {
      config.headers.Authorization = `Bearer ${authStore.token}`
    }
    return config
  },
  (error) => {
    return Promise.reject(error)
  }
)

// Response interceptor for error handling
api.interceptors.response.use(
  (response) => {
    return response
  },
  (error) => {
    if (error.response) {
      // Handle 401 Unauthorized - token expired or invalid
      if (error.response.status === 401) {
        const authStore = useAuthStore()
        authStore.clearAuth()
        router.push('/login')
      }
      
      // Handle 403 Forbidden - insufficient permissions
      if (error.response.status === 403) {
        console.error('Access denied:', error.response.data)
      }
      
      // Handle 404 Not Found
      if (error.response.status === 404) {
        console.error('Resource not found:', error.response.data)
      }
      
      // Handle 500 Internal Server Error
      if (error.response.status === 500) {
        console.error('Server error:', error.response.data)
      }
    } else if (error.request) {
      // Request was made but no response received
      console.error('No response from server:', error.request)
    } else {
      // Something else happened
      console.error('Error:', error.message)
    }
    
    return Promise.reject(error)
  }
)

export default api
