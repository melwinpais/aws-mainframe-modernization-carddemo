<template>
  <header class="app-header">
    <div class="header-content">
      <div class="header-left">
        <h1 class="app-title">CardDemo</h1>
        <nav class="breadcrumbs" v-if="breadcrumbs.length > 0">
          <span v-for="(crumb, index) in breadcrumbs" :key="index" class="breadcrumb">
            <router-link v-if="crumb.to" :to="crumb.to" class="breadcrumb-link">
              {{ crumb.label }}
            </router-link>
            <span v-else class="breadcrumb-current">{{ crumb.label }}</span>
            <span v-if="index < breadcrumbs.length - 1" class="breadcrumb-separator">/</span>
          </span>
        </nav>
      </div>
      <div class="header-right" v-if="isAuthenticated">
        <div class="user-info">
          <span class="user-name">{{ fullName }}</span>
          <span class="user-type">({{ userTypeLabel }})</span>
        </div>
        <button @click="handleLogout" class="logout-button">Logout</button>
      </div>
    </div>
  </header>
</template>

<script setup>
import { computed } from 'vue'
import { useRouter, useRoute } from 'vue-router'
import { useAuthStore } from '@/stores/auth'

const router = useRouter()
const route = useRoute()
const authStore = useAuthStore()

const isAuthenticated = computed(() => authStore.isAuthenticated)
const fullName = computed(() => `${authStore.firstName} ${authStore.lastName}`)
const userTypeLabel = computed(() => authStore.userType === 'A' ? 'Admin' : 'User')

const breadcrumbs = computed(() => {
  const crumbs = []
  
  if (route.meta.breadcrumbs) {
    return route.meta.breadcrumbs
  }
  
  // Default breadcrumb generation based on route
  if (route.path !== '/login' && route.path !== '/') {
    crumbs.push({ label: 'Menu', to: '/menu' })
    
    if (route.name && route.name !== 'MainMenu' && route.name !== 'AdminMenu') {
      crumbs.push({ label: route.meta.title || route.name })
    }
  }
  
  return crumbs
})

const handleLogout = () => {
  authStore.clearAuth()
  router.push('/login')
}
</script>

<style scoped>
.app-header {
  background-color: #2c3e50;
  color: white;
  padding: 1rem 2rem;
  box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1);
}

.header-content {
  display: flex;
  justify-content: space-between;
  align-items: center;
  max-width: 1200px;
  margin: 0 auto;
}

.header-left {
  display: flex;
  align-items: center;
  gap: 2rem;
}

.app-title {
  margin: 0;
  font-size: 1.5rem;
  font-weight: bold;
}

.breadcrumbs {
  display: flex;
  align-items: center;
  gap: 0.5rem;
  font-size: 0.9rem;
}

.breadcrumb {
  display: flex;
  align-items: center;
  gap: 0.5rem;
}

.breadcrumb-link {
  color: #3498db;
  text-decoration: none;
  transition: color 0.2s;
}

.breadcrumb-link:hover {
  color: #5dade2;
  text-decoration: underline;
}

.breadcrumb-current {
  color: #ecf0f1;
}

.breadcrumb-separator {
  color: #7f8c8d;
}

.header-right {
  display: flex;
  align-items: center;
  gap: 1rem;
}

.user-info {
  display: flex;
  flex-direction: column;
  align-items: flex-end;
  font-size: 0.9rem;
}

.user-name {
  font-weight: 500;
}

.user-type {
  color: #95a5a6;
  font-size: 0.8rem;
}

.logout-button {
  background-color: #e74c3c;
  color: white;
  border: none;
  padding: 0.5rem 1rem;
  border-radius: 4px;
  cursor: pointer;
  font-size: 0.9rem;
  transition: background-color 0.2s;
}

.logout-button:hover {
  background-color: #c0392b;
}

.logout-button:active {
  transform: translateY(1px);
}
</style>
