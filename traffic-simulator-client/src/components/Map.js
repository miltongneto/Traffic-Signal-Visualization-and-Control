import React from 'react'
import PropTypes from 'prop-types'
import GoogleMapReact from 'google-map-react'
import axios from 'axios'

import Signal from './Signal'

export class Map extends React.Component {
  static props = {
    onShowSignal: PropTypes.func.isRequired
  }

  static defaultProps = {
    center: {
      lat: -8.142323,
      lng: -34.903798
    },
    zoom: 13
  }

  state = {
    signals: []
  }

  componentDidMount() {
    setInterval(() => {
      this.getAllSignals()
    }, 1000)
    
  }

  render() {
    return (
      // Important! Always set the container height explicitly
      <div style={{ height: '100vh', width: '100%' }}>
        <GoogleMapReact
          bootstrapURLKeys={{ key: process.env.REACT_APP_GOOGLE_MAPS_KEY }}
          defaultCenter={this.props.center}
          defaultZoom={this.props.zoom}
          onChildClick={this.handleClick}
        >

          {this.state.signals.map(signal => 
            <Signal
              key={signal.trafficId}
              lat={signal.latitude}
              lng={signal.longitude}
              signal={signal}
            />
          )}
        </GoogleMapReact>
      </div>
    )
  }

  handleClick = (trafficId) => {
    this.props.onShowSignal(trafficId)
  }

  getAllSignals = async () => {
    try {
      const { data } = await axios.get(`${process.env.REACT_APP_API_URL}traffic-signal`)
      
      this.setState({
        signals: data
      })
    } catch(exception) {
      console.log(exception)
    }
  }
}

export default Map