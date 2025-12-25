<template>
  <div class="admin-menu">
    <div class="menu-container">
      <h1>Admin Menu</h1>
      <p class="welcome-message">Welcome, {{ firstName }} {{ lastName }} (Administrator)</p>
      
      <!-- Loading state -->
      <LoadingSpinner v-if="loading" />
      
      <!-- Error message -->
      <ErrorMessage v-if="errorMessage" :message="errorMessage" />
      
      <!-- Menu options -->
      <div v-if="!loading && menuOptions.length > 0" class="menu-options">
        <div class="menu-grid">
          <button
            v-for="option in menuOptions"
            :key="option.optionNumber"
            @click="selectMenuOption(option.optionNumber)"
            class="menu-option-button admin-option"
            :disabled="selecting"
          >
            <span class="option-number">{{ option.optionNumber }}</span>
            <span class="option-name">{{ option.optionName }}</span>
          </button>
        </div>
      </div>
      
      <!-- Action buttons -->
      <div class="action-buttons">
        <button @click="logout" class="logout-button" :disabled="selecting">
          Logout
        </button>
      </div>
    </div>
  </div>
</template>

<script>
import { ref, onMounted } from 'vue'
import { useAuthStore } from '../stores/auth'
import { useRouter } from 'vue-router'
import LoadingSpinner from '../components/LoadingSpinner.vue'
import ErrorMessage from '../components/ErrorMessage.vue'
import menuService from '../services/menuService'
import authService from '../services/authService'

export default {
  name: 'AdminMenu',
  components: {
    LoadingSpinner,
    ErrorMessage
  },
  setup() {
    const authStore = useAuthStore()
    const router = useRouter()

    const menuOptions = ref([])
    const loading = ref(false)
    const selecting = ref(false)
    const errorMessage = ref('')

    const firstName = ref(authStore.firstName)
    const lastName = ref(authStore.lastName)

    /**
     * Load admin menu options for the authenticated admin user
     */
    const loadMenuOptions = async () => {
      loading.value = true
      errorMessage.value = ''
      
      try {
        const options = await menuService.getOptions()
        menuOptions.value = options
      } catch (error) {
        console.error('Error loading admin menu options:', error)
        errorMessage.value = error.response?.data?.message || 
                            'Failed to load admin menu options. Please try again.'
      } finally {
        loading.value = false
      }
    }

    /**
     * Select a menu option and navigate to the corresponding feature
     */
    const selectMenuOption = async (optionNumber) => {
      selecting.value = true
      errorMessage.value = ''
      
      try {
        const response = await menuService.selectOption(optionNumber)
        
        // Navigate to the route returned by the backend
        if (response.route) {
          router.push(response.route)
        } else {
          errorMessage.value = 'Invalid menu option selected'
        }
      } catch (error) {
        console.error('Error selecting menu option:', error)
        
        // Handle specific error messages
        if (error.response?.data?.message) {
          errorMessage.value = error.response.data.message
        } else if (error.response?.status === 400) {
          errorMessage.value = 'Please enter a valid option number...'
        } else {
          errorMessage.value = 'Failed to process menu selection. Please try again.'
        }
      } finally {
        selecting.value = false
      }
    }

    /**
     * Logout the user and navigate to login page
     */
    const logout = async () => {
      try {
        await authService.logout()
      } catch (error) {
        console.error('Error during logout:', error)
      } finally {
        authStore.clearAuth()
        router.push('/login')
      }
    }

    // Load menu options when component mounts
    onMounted(() => {
      loadMenuOptions()
    })

    return {
      menuOptions,
      loading,
      selecting,
      errorMessage,
      firstName,
      lastName,
      selectMenuOption,
      logout
    }
  }
}
</script>

<style scoped>
.admin-menu {
  min-height: 100vh;
  background-color: #f5f5f5;
}

.menu-container {
  max-width: 900px;
  margin: 0 auto;
  padding: 2rem;
  background-color: white;
  border-radius: 8px;
  box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
}

h1 {
  color: #333;
  margin-bottom: 0.5rem;
  font-size: 2rem;
}

.welcome-message {
  color: #666;
  margin-bottom: 2rem;
  font-size: 1.1rem;
}

.menu-options {
  margin: 2rem 0;
}

.menu-grid {
  display: grid;
  grid-template-columns: repeat(auto-fill, minmax(280px, 1fr));
  gap: 1rem;
}

.menu-option-button {
  display: flex;
  align-items: center;
  gap: 1rem;
  padding: 1rem 1.5rem;
  color: white;
  border: none;
  border-radius: 6px;
  cursor: pointer;
  font-size: 1rem;
  text-align: left;
  transition: all 0.2s ease;
}

.admin-option {
  background-color: #dc3545;
}

.admin-option:hover:not(:disabled) {
  background-color: #c82333;
  transform: translateY(-2px);
  box-shadow: 0 4px 8px rgba(0, 0, 0, 0.15);
}

.menu-option-button:active:not(:disabled) {
  transform: translateY(0);
}

.menu-option-button:disabled {
  opacity: 0.6;
  cursor: not-allowed;
}

.option-number {
  display: inline-flex;
  align-items: center;
  justify-content: center;
  width: 32px;
  height: 32px;
  background-color: rgba(255, 255, 255, 0.2);
  border-radius: 50%;
  font-weight: bold;
  font-size: 1.1rem;
}

.option-name {
  flex: 1;
  font-weight: 500;
}

.action-buttons {
  margin-top: 2rem;
  padding-top: 2rem;
  border-top: 1px solid #e0e0e0;
  display: flex;
  justify-content: flex-end;
}

.logout-button {
  background-color: #6c757d;
  color: white;
  border: none;
  padding: 0.75rem 2rem;
  border-radius: 4px;
  cursor: pointer;
  font-size: 1rem;
  transition: background-color 0.2s ease;
}

.logout-button:hover:not(:disabled) {
  background-color: #5a6268;
}

.logout-button:disabled {
  opacity: 0.6;
  cursor: not-allowed;
}
</style>
