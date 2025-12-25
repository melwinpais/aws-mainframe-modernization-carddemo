import { defineStore } from 'pinia'
import { ref, computed } from 'vue'

export const useAuthStore = defineStore('auth', () => {
  const token = ref(localStorage.getItem('token') || null)
  const userId = ref(localStorage.getItem('userId') || null)
  const userType = ref(localStorage.getItem('userType') || null)
  const firstName = ref(localStorage.getItem('firstName') || null)
  const lastName = ref(localStorage.getItem('lastName') || null)

  const isAuthenticated = computed(() => !!token.value)
  const isAdmin = computed(() => userType.value === 'A')

  function setAuth(authData) {
    token.value = authData.token
    userId.value = authData.userId
    userType.value = authData.userType
    firstName.value = authData.firstName
    lastName.value = authData.lastName

    localStorage.setItem('token', authData.token)
    localStorage.setItem('userId', authData.userId)
    localStorage.setItem('userType', authData.userType)
    localStorage.setItem('firstName', authData.firstName)
    localStorage.setItem('lastName', authData.lastName)
  }

  function clearAuth() {
    token.value = null
    userId.value = null
    userType.value = null
    firstName.value = null
    lastName.value = null

    localStorage.removeItem('token')
    localStorage.removeItem('userId')
    localStorage.removeItem('userType')
    localStorage.removeItem('firstName')
    localStorage.removeItem('lastName')
  }

  return {
    token,
    userId,
    userType,
    firstName,
    lastName,
    isAuthenticated,
    isAdmin,
    setAuth,
    clearAuth
  }
})
