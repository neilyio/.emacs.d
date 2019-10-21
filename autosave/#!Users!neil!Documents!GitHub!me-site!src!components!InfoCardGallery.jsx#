import React from "react";
import styled from "styled-components";
import { InfoCardContainer } from "./InfoCard.jsx";

const Container = styled.div`
    display: flex;
    flex-direction: row;
    justify-content: space-between;
    width: 100%;

    & > ${InfoCardContainer} {
        width: calc(1/3*100% - 5px);
    }

`;

const InfoCardGallery = ({ children }) => {
  return (
      <Container>
        { children }
      </Container>
  );
};

export default InfoCardGallery;
