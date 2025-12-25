import { createRouter, createWebHistory } from 'vue-router'
import { useAuthStore } from '@/stores/auth'

const routes = [
  {
    path: '/',
    redirect: '/login'
  },
  {
    path: '/login',
    name: 'Login',
    component: () => import('@/views/Login.vue'),
    meta: { title: 'Sign On' }
  },
  {
    path: '/menu',
    name: 'MainMenu',
    component: () => import('@/views/MainMenu.vue'),
    meta: { 
      requiresAuth: true,
      title: 'Main Menu'
    }
  },
  {
    path: '/admin-menu',
    name: 'AdminMenu',
    component: () => import('@/views/AdminMenu.vue'),
    meta: { 
      requiresAuth: true,
      requiresAdmin: true,
      title: 'Admin Menu'
    }
  },
  {
    path: '/accounts/view',
    name: 'AccountView',
    component: () => import('@/views/AccountView.vue'),
    meta: { 
      requiresAuth: true,
      title: 'Account View',
      breadcrumbs: [
        { label: 'Menu', to: '/menu' },
        { label: 'Account View' }
      ]
    }
  },
  {
    path: '/accounts/update',
    name: 'AccountUpdate',
    component: () => import('@/views/AccountUpdate.vue'),
    meta: { 
      requiresAuth: true,
      title: 'Account Update',
      breadcrumbs: [
        { label: 'Menu', to: '/menu' },
        { label: 'Account View', to: '/accounts/view' },
        { label: 'Account Update' }
      ]
    }
  },
  {
    path: '/cards/list',
    name: 'CardList',
    component: () => import('@/views/CardList.vue'),
    meta: { 
      requiresAuth: true,
      title: 'Card List',
      breadcrumbs: [
        { label: 'Menu', to: '/menu' },
        { label: 'Card List' }
      ]
    }
  },
  {
    path: '/cards/detail/:cardNumber',
    name: 'CardDetail',
    component: () => import('@/views/CardDetail.vue'),
    meta: { 
      requiresAuth: true,
      title: 'Card Detail',
      breadcrumbs: [
        { label: 'Menu', to: '/menu' },
        { label: 'Card List', to: '/cards/list' },
        { label: 'Card Detail' }
      ]
    }
  },
  {
    path: '/cards/update/:cardNumber',
    name: 'CardUpdate',
    component: () => import('@/views/CardUpdate.vue'),
    meta: { 
      requiresAuth: true,
      title: 'Card Update',
      breadcrumbs: [
        { label: 'Menu', to: '/menu' },
        { label: 'Card List', to: '/cards/list' },
        { label: 'Card Update' }
      ]
    }
  },
  {
    path: '/transactions/list',
    name: 'TransactionList',
    component: () => import('@/views/TransactionList.vue'),
    meta: { 
      requiresAuth: true,
      title: 'Transaction List',
      breadcrumbs: [
        { label: 'Menu', to: '/menu' },
        { label: 'Transaction List' }
      ]
    }
  },
  {
    path: '/transactions/view/:transactionId',
    name: 'TransactionView',
    component: () => import('@/views/TransactionView.vue'),
    meta: { 
      requiresAuth: true,
      title: 'Transaction View',
      breadcrumbs: [
        { label: 'Menu', to: '/menu' },
        { label: 'Transaction List', to: '/transactions/list' },
        { label: 'Transaction View' }
      ]
    }
  },
  {
    path: '/bills/payment',
    name: 'BillPayment',
    component: () => import('@/views/BillPayment.vue'),
    meta: { 
      requiresAuth: true,
      title: 'Bill Payment',
      breadcrumbs: [
        { label: 'Menu', to: '/menu' },
        { label: 'Bill Payment' }
      ]
    }
  },
  {
    path: '/reports',
    name: 'Reports',
    component: () => import('@/views/Reports.vue'),
    meta: { 
      requiresAuth: true,
      title: 'Reports',
      breadcrumbs: [
        { label: 'Menu', to: '/menu' },
        { label: 'Reports' }
      ]
    }
  },
  {
    path: '/users',
    name: 'UserManagement',
    component: () => import('@/views/UserManagement.vue'),
    meta: { 
      requiresAuth: true,
      requiresAdmin: true,
      title: 'User Management',
      breadcrumbs: [
        { label: 'Admin Menu', to: '/admin-menu' },
        { label: 'User Management' }
      ]
    }
  },
  {
    path: '/authorizations',
    name: 'Authorizations',
    component: () => import('@/views/Authorizations.vue'),
    meta: { 
      requiresAuth: true,
      title: 'Pending Authorizations',
      breadcrumbs: [
        { label: 'Menu', to: '/menu' },
        { label: 'Pending Authorizations' }
      ]
    }
  }
]

const router = createRouter({
  history: createWebHistory(import.meta.env.BASE_URL),
  routes
})

// Navigation guard for authentication
router.beforeEach((to, from, next) => {
  const authStore = useAuthStore()
  const requiresAuth = to.matched.some(record => record.meta.requiresAuth)
  const requiresAdmin = to.matched.some(record => record.meta.requiresAdmin)
  
  if (requiresAuth && !authStore.isAuthenticated) {
    // Redirect to login if authentication is required but user is not authenticated
    next('/login')
  } else if (requiresAdmin && authStore.userType !== 'A') {
    // Redirect to main menu if admin access is required but user is not admin
    next('/menu')
  } else {
    // Allow navigation
    next()
  }
})

export default router
