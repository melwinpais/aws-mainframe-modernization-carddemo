package com.carddemo.security;

import io.jsonwebtoken.*;
import io.jsonwebtoken.security.Keys;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import javax.crypto.SecretKey;
import java.nio.charset.StandardCharsets;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;

/**
 * JWT Token Provider for generating, validating, and parsing JWT tokens.
 * Handles session management for authenticated users.
 */
@Component
public class JwtTokenProvider {

    private static final Logger logger = LoggerFactory.getLogger(JwtTokenProvider.class);

    @Value("${jwt.secret}")
    private String jwtSecret;

    @Value("${jwt.expiration:3600000}") // Default 1 hour
    private long jwtExpiration;

    /**
     * Generate JWT token for authenticated user
     *
     * @param userId User ID
     * @param userType User type ('A' for Admin, 'U' for User)
     * @param firstName User's first name
     * @param lastName User's last name
     * @return JWT token string
     */
    public String generateToken(String userId, String userType, String firstName, String lastName) {
        Date now = new Date();
        Date expiryDate = new Date(now.getTime() + jwtExpiration);

        Map<String, Object> claims = new HashMap<>();
        claims.put("userId", userId);
        claims.put("userType", userType);
        claims.put("firstName", firstName);
        claims.put("lastName", lastName);

        SecretKey key = Keys.hmacShaKeyFor(jwtSecret.getBytes(StandardCharsets.UTF_8));

        String token = Jwts.builder()
                .setClaims(claims)
                .setSubject(userId)
                .setIssuedAt(now)
                .setExpiration(expiryDate)
                .signWith(key, SignatureAlgorithm.HS512)
                .compact();

        logger.info("Generated JWT token for user: {}", userId);
        return token;
    }

    /**
     * Validate JWT token
     *
     * @param token JWT token string
     * @return true if token is valid, false otherwise
     */
    public boolean validateToken(String token) {
        try {
            SecretKey key = Keys.hmacShaKeyFor(jwtSecret.getBytes(StandardCharsets.UTF_8));
            Jwts.parser()
                    .verifyWith(key)
                    .build()
                    .parseSignedClaims(token);
            return true;
        } catch (SecurityException ex) {
            logger.error("Invalid JWT signature");
        } catch (MalformedJwtException ex) {
            logger.error("Invalid JWT token");
        } catch (ExpiredJwtException ex) {
            logger.error("Expired JWT token");
        } catch (UnsupportedJwtException ex) {
            logger.error("Unsupported JWT token");
        } catch (IllegalArgumentException ex) {
            logger.error("JWT claims string is empty");
        }
        return false;
    }

    /**
     * Get user ID from JWT token
     *
     * @param token JWT token string
     * @return User ID
     */
    public String getUserIdFromToken(String token) {
        Claims claims = getClaimsFromToken(token);
        return claims.getSubject();
    }

    /**
     * Get user type from JWT token
     *
     * @param token JWT token string
     * @return User type ('A' or 'U')
     */
    public String getUserTypeFromToken(String token) {
        Claims claims = getClaimsFromToken(token);
        return claims.get("userType", String.class);
    }

    /**
     * Get first name from JWT token
     *
     * @param token JWT token string
     * @return User's first name
     */
    public String getFirstNameFromToken(String token) {
        Claims claims = getClaimsFromToken(token);
        return claims.get("firstName", String.class);
    }

    /**
     * Get last name from JWT token
     *
     * @param token JWT token string
     * @return User's last name
     */
    public String getLastNameFromToken(String token) {
        Claims claims = getClaimsFromToken(token);
        return claims.get("lastName", String.class);
    }

    /**
     * Parse JWT token and extract claims
     *
     * @param token JWT token string
     * @return Claims object containing token data
     */
    private Claims getClaimsFromToken(String token) {
        SecretKey key = Keys.hmacShaKeyFor(jwtSecret.getBytes(StandardCharsets.UTF_8));
        return Jwts.parser()
                .verifyWith(key)
                .build()
                .parseSignedClaims(token)
                .getPayload();
    }

    /**
     * Get token expiration time in milliseconds
     *
     * @return Expiration time in milliseconds
     */
    public long getExpirationTime() {
        return jwtExpiration;
    }
}
