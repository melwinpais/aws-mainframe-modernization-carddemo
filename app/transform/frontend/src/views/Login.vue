<template>
  <div class="login-container">
    <div class="login-card">
      <h1>CardDemo</h1>
      <h2>Credit Card Management System</h2>
      
      <form @submit.prevent="handleLogin">
        <div class="form-group">
          <label for="userId">User ID</label>
          <input
            id="userId"
            v-model="userId"
            type="text"
            placeholder="Enter User ID"
            maxlength="8"
            required
          />
        </div>
        
        <div class="form-group">
          <label for="password">Password</label>
          <input
            id="password"
            v-model="password"
            type="password"
            placeholder="Enter Password"
            maxlength="8"
            required
          />
        </div>
        
        <div v-if="errorMessage" class="error-message">
          {{ errorMessage }}
        </div>
        
        <button type="submit" :disabled="loading">
          {{ loading ? 'Signing in...' : 'Sign In' }}
        </button>
      </form>
    </div>
  </div>
</template>

<script>
import authService from '@/services/authService'
import { useRouter } from 'vue-router'

export default {
  name: 'Login',
  setup() {
    const router = useRouter()
    return { router }
  },
  data() {
    return {
      userId: '',
      password: '',
      errorMessage: '',
      loading: false
    }
  },
  methods: {
    /**
     * Validate form inputs before submission
     * Requirements: 1.5, 1.6, 15.2, 15.3
     */
    validateForm() {
      // Clear previous error
      this.errorMessage = ''
      
      // Validate user ID
      const trimmedUserId = this.userId.trim()
      if (!trimmedUserId) {
        this.errorMessage = 'Please enter User ID ...'
        return false
      }
      
      // Validate password
      const trimmedPassword = this.password.trim()
      if (!trimmedPassword) {
        this.errorMessage = 'Please enter Password ...'
        return false
      }
      
      return true
    },

    /**
     * Handle login form submission
     * Requirements: 1.1, 1.2, 1.3, 1.4, 1.5, 1.6, 15.2, 15.3
     */
    async handleLogin() {
      // Validate form inputs
      if (!this.validateForm()) {
        return
      }
      
      this.loading = true
      this.errorMessage = ''
      
      try {
        // Call authentication service
        const response = await authService.login(this.userId, this.password)
        
        // Navigate to appropriate menu based on user type
        // Requirements: 1.7, 1.8
        if (response.userType === 'A') {
          // Admin user - navigate to admin menu
          this.router.push('/admin-menu')
        } else {
          // Regular user - navigate to main menu
          this.router.push('/menu')
        }
      } catch (error) {
        // Display error message
        // Requirements: 1.3, 1.4, 15.3
        this.errorMessage = error.message
      } finally {
        this.loading = false
      }
    }
  }
}
</script>

<style scoped>
.login-container {
  display: flex;
  justify-content: center;
  align-items: center;
  min-height: 100vh;
  background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
}

.login-card {
  background: white;
  padding: 2rem;
  border-radius: 8px;
  box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
  width: 100%;
  max-width: 400px;
}

h1 {
  margin: 0 0 0.5rem 0;
  color: #333;
  text-align: center;
}

h2 {
  margin: 0 0 2rem 0;
  font-size: 1rem;
  font-weight: normal;
  color: #666;
  text-align: center;
}

.form-group {
  margin-bottom: 1.5rem;
}

label {
  display: block;
  margin-bottom: 0.5rem;
  color: #333;
  font-weight: 500;
}

input {
  width: 100%;
  padding: 0.75rem;
  border: 1px solid #ddd;
  border-radius: 4px;
  font-size: 1rem;
  box-sizing: border-box;
}

input:focus {
  outline: none;
  border-color: #667eea;
}

button {
  width: 100%;
  padding: 0.75rem;
  background: #667eea;
  color: white;
  border: none;
  border-radius: 4px;
  font-size: 1rem;
  font-weight: 500;
  cursor: pointer;
  transition: background 0.3s;
}

button:hover:not(:disabled) {
  background: #5568d3;
}

button:disabled {
  background: #ccc;
  cursor: not-allowed;
}

.error-message {
  color: #e53e3e;
  background: #fff5f5;
  border: 1px solid #feb2b2;
  padding: 0.75rem;
  border-radius: 4px;
  margin-bottom: 1rem;
  font-size: 0.875rem;
}
</style>
