# StackEstate Smart Contract

A Clarity smart contract for real estate tokenization on the Stacks blockchain, enabling fractional ownership, primary & secondary markets, and automated rent distribution.

## Features

- 🏠 **Property Tokenization**: Convert real estate into tradeable digital shares
- 💰 **Dual Markets**: Support for both primary sales and P2P secondary trading
- 📈 **Dynamic Pricing**: Configurable price-per-share with owner controls
- 🔄 **Automated Distribution**: Slice-based rent distribution to shareholders
- 🔐 **Access Control**: Multi-level authorization with owner and operator roles
- ⚡ **Gas Efficient**: Optimized for minimal transaction costs
- 🛡️ **Safety Features**: Pause mechanism and transaction guards

## Contract Functions

### Property Management

```clarity
(mint-property (name (string-ascii 50)) 
               (location (string-ascii 100)) 
               (total-shares uint) 
               (price-per-share uint) 
               (min-chunk uint))

(set-for-sale (property-id uint) (status bool))
(update-price (property-id uint) (new-price uint))
(deactivate-property (property-id uint) (status bool))
```

### Trading Functions

```clarity
;; Primary Market
(buy-shares (property-id uint) (num-shares uint))

;; Secondary Market
(list-shares (property-id uint) (shares uint) (price-per-share uint))
(cancel-listing (listing-id uint))
(buy-from-listing (listing-id uint) (num-shares uint))
```

### Rent Distribution

```clarity
(begin-distribution (property-id uint) (total-rent uint))
(distribute-slice (property-id uint) (max-steps uint))
```

## Administrative Functions

```clarity
(set-operator (who principal) (is-op bool))
(set-fee-bps (new-bps uint))
(set-treasury (to principal))
(set-paused (state bool))
```

## Error Codes

| Code | Description |
|------|-------------|
| 401 | Not owner |
| 402 | Unauthorized |
| 400 | Bad request |
| 404 | Not found |
| 405 | Not for sale |
| 406 | Insufficient funds/shares |
| 407 | No rent available |
| 408 | Contract paused |
| 409 | Listing closed |
| 410 | Invalid state |
| 411 | Duplicate entry |

## Installation

1. Install [Clarinet](https://github.com/hirosystems/clarinet)
2. Clone this repository
3. Deploy using Clarinet or your preferred Stacks deployment tool

## Testing

```bash
clarinet test
```

## Security Considerations

- All monetary operations use safe math
- Access control checks on sensitive functions
- Emergency pause mechanism
- Rate limiting on investor registry
- Protected distribution state

## Disclaimer

This smart contract is provided as-is. Users should perform their own security audit before using in production.
