<template>
  <div class="authorizations">
    <div class="container">
      <h2>Pending Authorizations</h2>
      
      <div class="info-message">
        <p>No pending authorizations at this time.</p>
        <p>This page would typically show:</p>
        <ul>
          <li>Pending transaction authorizations</li>
          <li>Credit limit increase requests</li>
          <li>Card activation requests</li>
          <li>Account modification approvals</li>
        </ul>
      </div>

      <div class="actions">
        <button class="btn btn-secondary" @click="returnToMenu">
          Return to Menu
        </button>
      </div>
    </div>
  </div>
</template>

<script>
import { useRouter } from 'vue-router'

export default {
  name: 'Authorizations',
  setup() {
    const router = useRouter()

    const returnToMenu = () => {
      router.push('/menu')
    }

    return {
      returnToMenu
    }
  }
}
</script>

<style scoped>
.authorizations {
  padding: 20px;
}

.container {
  max-width: 800px;
  margin: 0 auto;
}

.info-message {
  background-color: #f8f9fa;
  border: 1px solid #dee2e6;
  border-radius: 4px;
  padding: 20px;
  margin: 20px 0;
}

.info-message ul {
  margin: 10px 0 0 20px;
}

.actions {
  margin-top: 20px;
}

.btn {
  padding: 8px 16px;
  border: none;
  border-radius: 4px;
  cursor: pointer;
  text-decoration: none;
  display: inline-block;
}

.btn-secondary {
  background-color: #6c757d;
  color: white;
}

.btn-secondary:hover {
  background-color: #5a6268;
}
</style>
